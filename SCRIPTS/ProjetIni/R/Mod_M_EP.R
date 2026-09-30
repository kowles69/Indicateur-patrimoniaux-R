# R/Mod_M_EP.R — Module Métropole — EP (eaux pluviales)
# Mise en page homogène avec EU :
# - Onglets Collecteurs / Branchements / Ouvrages
# - Tableau Ouvrages façon PDF + donuts Age Bassins / Age STR (EP)
# - Réseaux secs (HT/BT) + précision texte
# - État de santé : Collecteurs (donuts + encarts texte), Ouvrages (inspections + donut ES_STR_EP)

library(dplyr)
library(plotly)
library(magrittr)

# ---------------- utils communs ----------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x  # <-- évite le crash

.ep_num <- function(x) suppressWarnings(as.numeric(x))

.ep_fmt_km <- function(x, digits = 2) {
  v <- .ep_num(x)
  if (length(v) == 0 || !is.finite(v)) return("—")
  paste0(format(round(v, digits), big.mark = " ", decimal.mark = ","), " km")
}

.ep_fmt_m <- function(x, digits = 0) {
  v <- .ep_num(x)
  if (length(v) == 0 || !is.finite(v)) return("—")
  paste0(format(round(v, digits), big.mark = " ", decimal.mark = ","), " m")
}

.ep_fmt_eur <- function(x) {
  v <- .ep_num(x)
  if (length(v) == 0 || !is.finite(v)) return("—")
  paste0(format(round(v, 0), big.mark = " ", decimal.mark = ","), " €")
}

.ep_info_card <- function(title, ..., class = "") {
  div(class = paste("eu-infocard", class),
      div(class = "eu-infocard-title", title),
      pre(class  = "eu-infocard-body", paste0(..., collapse = "")))
}

# récupère un data.frame d’une clé de section
.ep_get <- function(sec, key) {
  obj <- sec[[key]]
  if (is.null(obj)) return(NULL)
  as.data.frame(obj)
}

# récupère une colonne avec tolérance sur les noms (exact puis pattern)
.pickcol <- function(df, choices_regex) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NA_real_)
  nms <- names(df)
  # exact
  for (c in choices_regex) {
    idx <- which(tolower(nms) == tolower(c))
    if (length(idx)) return(df[[ idx[1] ]])
  }
  # pattern
  for (c in choices_regex) {
    idx <- grep(c, nms, ignore.case = TRUE)
    if (length(idx)) return(df[[ idx[1] ]])
  }
  NA_real_
}

.ep_plot_theme <- function() {
  list(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    margin = list(l = 6, r = 6, t = 35, b = 0)   # top assez grand pour ne pas couper les titres
  )
}

# Donut avec % + valeur (km = FALSE pour des "nombres")
.ep_donut <- function(values, labels, title = "", unit_km = FALSE, min_pct_label = 0.045) {
  values <- .ep_num(values)
  ok <- is.finite(values) & values >= 0
  values <- values[ok]; labels <- labels[ok]

  if (length(values) == 0 || sum(values) == 0) {
    values <- 1; labels <- "Aucune donnée"
  }

  tot <- sum(values)
  pct <- values / tot
  value_txt <- if (unit_km)
    paste0(format(round(values, 2), big.mark = " ", decimal.mark = ","), " km")
  else
    format(round(values, 0), big.mark = " ", decimal.mark = ",")
  txt <- ifelse(pct >= min_pct_label,
                paste0(round(pct * 100, 1), "%<br>", value_txt),
                "")

  plot_ly(
    type = "pie",
    labels = labels,
    values = values,
    text = txt,
    textposition = "inside",
    textinfo = "text",
    hovertemplate = "%{label}<br>%{value} (%{percent})<extra></extra>",
    hole = 0.62,
    sort = FALSE,
    marker = list(line = list(color = "white", width = 1))
  ) %>%
    layout(
      title = list(text = title, font = list(size = 14, family = "Arial Black")),
      legend = list(orientation = "v"),
      margin = list(l = 5, r = 5, t = 28, b = 5)
    )
}

# petite barre empilée horizontale pour "connaissance"
.ep_single_hstack <- function(values, labels, title = "") {
  values <- .ep_num(values)
  ok <- is.finite(values) & values >= 0
  values <- values[ok]; labels <- labels[ok]
  if (length(values) == 0 || sum(values) == 0) {
    values <- 1; labels <- "Aucune donnée"
  }
  tot <- sum(values)
  pct <- values / tot

  p <- plot_ly(orientation = "h")
  for (i in seq_along(values)) {
    p <- add_trace(
      p,
      type = "bar",
      x = values[i],
      y = "",
      name = labels[i],
      text = paste0(round(pct[i] * 100, 1), "% — ",
                    format(round(values[i], 2), big.mark = " ", decimal.mark = ",")),
      textposition = "inside",
      texttemplate = "%{text}",
      hovertemplate = "%{fullData.name}<br>%{x} (%{percent})<extra></extra>",
      marker = list(line = list(color = "white", width = 1)),
      showlegend = TRUE
    )
  }
  p %>% layout(
    barmode = "stack",
    title = list(text = title, font = list(size = 14, family = "Arial Black")),
    xaxis = list(title = NULL),
    yaxis = list(title = NULL, showticklabels = FALSE),
    margin = list(l = 5, r = 5, t = 28, b = 5)
  )
}

# Histogramme horizontal matériaux (classé décroissant)
.ep_bar_materials <- function(values, labels, title = "") {
  values <- .ep_num(values)
  ok <- is.finite(values) & values >= 0
  values <- values[ok]; labels <- labels[ok]
  if (length(values) == 0) { values <- 0; labels <- "Aucune donnée" }

  ord <- order(values, decreasing = TRUE)
  values <- values[ord]; labels <- labels[ord]
  y_levels <- labels[order(values, decreasing = TRUE)]
  y_fac    <- factor(labels, levels = y_levels)

  tot <- sum(values)
  pct <- if (tot > 0) round(values / tot * 100, 1) else rep(0, length(values))
  text <- paste0(pct, "% — ", format(round(values, 2), big.mark = " ", decimal.mark = ","), " km")

  thm <- .ep_plot_theme()

  plot_ly(
    x = values, y = y_fac, type = "bar", orientation = "h",
    text = text, textposition = "auto",
    hovertemplate = "%{y}<br>%{x} km (%{text})<extra></extra>"
  ) %>%
    layout(
      title = list(text = title, font = list(size = 14, family = "Arial Black")),
      xaxis = list(title = NULL),
      yaxis = list(
        title = NULL, automargin = TRUE,
        categoryorder = "array", categoryarray = rev(y_levels)
      ),
      paper_bgcolor = thm$paper_bgcolor,
      plot_bgcolor  = thm$plot_bgcolor,
      margin = list(l = 5, r = 10, t = 28, b = 5)
    )
}

# format "x (p%)" pour récolements si dispo
.fmt_recol <- function(df) {
  if (is.null(df)) return("—")
  rec  <- .pickcol(df, c("nbre_rec","^nbre.?rec$"))
  pct  <- .pickcol(df, c("^Pourcentage","Pourcentage_.*"))
  if (is.finite(.ep_num(rec)) && is.finite(.ep_num(pct))) {
    paste0(rec, " (", round(.ep_num(pct), 0), "%)")
  } else if (is.finite(.ep_num(rec))) {
    as.character(rec)
  } else {
    "—"
  }
}

# ---------------- UI ----------------

mod_M_EP_ui <- function(id, title = "Métropole — EP") {
  ns <- NS(id)

  tagList(
    # Agrandir le 1er tableau (inventaire > collecteurs)
    tags$style(HTML(
      sprintf("#%s table{font-size:14px;} #%s{max-width:760px;}", ns("tbl_lineaire"), ns("tbl_lineaire"))
    )),

    uiOutput(ns("crumb"), container = div, class = "eu-crumb"),

    tabsetPanel(
      id   = ns("main"),
      type = "pills",

      # INVENTAIRE
      tabPanel(
        value = "inv",
        tagList(icon("folder-open")), "Inventaire des patrimoines",

        tabsetPanel(
          id = ns("inv_sub"),
          type = "pills",

          # Collecteurs
          tabPanel(
            value = "inv_collect",
            tagList(icon("project-diagram")), "Collecteurs",
            br(),
            # LIGNE 1
            fluidRow(
              column(3, tableOutput(ns("tbl_lineaire"))),
              column(3,
                     plotlyOutput(ns("plt_connaissance_mat_bar"), height = "120px"),
                     plotlyOutput(ns("plt_connaissance_age_bar"), height = "120px")
              ),
              column(3, plotlyOutput(ns("plt_tranches_age_total"), height = "260px")),
              column(3, plotlyOutput(ns("plt_precision"), height = "260px"))
            ),
            # LIGNE 2
            fluidRow(
              column(9, plotlyOutput(ns("plt_materiaux_bar"), height = "340px")),
              column(3, uiOutput(ns("cards_valeurs")))
            )
          ),

          # Branchements
          tabPanel(
            value = "inv_branch",
            tagList(icon("plug")), "Branchements",
            br(),
            fluidRow(
              column(6, plotlyOutput(ns("plt_br_ce"), height = "300px")),
              column(6, tableOutput(ns("tbl_br")))
            )
          ),

          # Ouvrages
          tabPanel(
            value = "inv_ouvr",
            tagList(icon("cogs")), "Ouvrages",
            br(),
            tableOutput(ns("tbl_ouvrages")),
            br(),
            # les 2 donuts d'âge ENTRE le tableau et les cartes réseaux secs
            fluidRow(
              column(6, plotlyOutput(ns("plt_age_bassins"), height = "300px")),
              column(6, plotlyOutput(ns("plt_age_str"),     height = "300px"))
            ),
            br(),
            fluidRow(
              column(6, uiOutput(ns("card_reseau_secs"))),
              column(6, uiOutput(ns("card_precision_secs")))
            )
          )
        )
      ),

      # ETAT DE SANTE
      tabPanel(
        value = "sante",
        tagList(icon("heart-pulse")), "État de santé",

        tabsetPanel(
          id = ns("sante_sub"),
          type = "pills",

          tabPanel(
            value = "sante_collect",
            tagList(icon("project-diagram")), "Collecteurs",
            br(),
            fluidRow(
              column(6, plotlyOutput(ns("plt_etat_nv"), height = "300px")),
              column(6, plotlyOutput(ns("plt_etat_v"),  height = "300px"))
            ),
            br(),
            # Encarts texte demandés (ITV / DIAG / PRÉDIAG)
            fluidRow(
              column(4, uiOutput(ns("card_litv"))),
              column(4, uiOutput(ns("card_diag"))),
              column(4, uiOutput(ns("card_prediag")))
            )
          ),

          tabPanel(
            value = "sante_branch",
            tagList(icon("plug")), "Branchements",
            br(),
            div(class="muted", "Pas de données connues pour l’instant.")
          ),

          tabPanel(
            value = "sante_ouvr",
            tagList(icon("cogs")), "Ouvrages",
            br(),
            tableOutput(ns("tbl_inspections")),
            br(),
            plotlyOutput(ns("plt_es_str_ep"), height = "260px")  # donut ES_STR_EP
          )
        )
      ),

      # PROGRAMME DE TRAVAUX
      tabPanel(
        value = "travaux",
        tagList(icon("wrench")), "Programme de travaux",
        br(),
        uiOutput(ns("card_travaux"))
      ),

      # CARTO
      tabPanel(
        value = "carto",
        tagList(icon("globe")), "Cartographie",
        br(),
        div(class = "muted", "À venir.")
      )
    )
  )
}

# ---------------- SERVER ----------------

mod_M_EP_server <- function(id, store_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sec <- reactive({
      req(store_list)
      # essaie plusieurs variantes de clé
      for (k in c("Metropole_EP","Métropole_EP","Metropole EP","M_EP")) {
        s <- try(get_section(store_list, k), silent = TRUE)
        if (!inherits(s, "try-error") && !is.null(s)) return(s)
      }
      NULL
    })

    # fil d’ariane
    output$crumb <- renderUI({
      main_map <- c(
        inv = "Inventaire des patrimoines",
        sante = "État de santé",
        travaux = "Programme de travaux",
        carto = "Cartographie"
      )
      sub_map <- switch(
        input$main %||% "inv",
        inv   = c(inv_collect="Collecteurs", inv_branch="Branchements", inv_ouvr="Ouvrages"),
        sante = c(sante_collect="Collecteurs", sante_branch="Branchements", sante_ouvr="Ouvrages"),
        travaux = c(), carto = c()
      )
      main_lbl <- main_map[[ input$main %||% "inv" ]]
      sub_id   <- if (input$main == "inv") input$inv_sub else if (input$main=="sante") input$sante_sub else NULL
      sub_lbl  <- if (length(sub_map)) sub_map[[ sub_id %||% names(sub_map)[1] ]] else NULL

      tagList(
        span(class="crumb-main",   main_lbl),
        if (!is.null(sub_lbl)) span(class="crumb-sep", icon("angle-right")),
        if (!is.null(sub_lbl)) span(class="crumb-sub",  sub_lbl)
      )
    })

    # -------- Inventaire > Collecteurs --------

    output$tbl_lineaire <- renderTable({
      s <- sec(); req(s)
      # EP : on affiche uniquement visitable / non visitable / Total
      lrv   <- .ep_get(s,"LRV")
      lep   <- .ep_get(s,"LEP")
      visit <- .pickcol(lrv, c("longueur_visitable","visitable"))
      nvis  <- .pickcol(lrv, c("longueur_non_visitable","non.?visitable"))
      tot   <- .pickcol(lep, c("longueur_tot","total","longueur_sum","LEP"))
      tibble::tibble(
        Libellé     = c("Réseaux visitables (>=1,5m)", "Réseaux non visitables", "Total"),
        `Valeur`    = c(.ep_fmt_km(visit), .ep_fmt_km(nvis), .ep_fmt_km(tot))
      )
    }, striped = TRUE, hover = TRUE, spacing = 'xs', digits = 2)

    # barres empilées connaissance matériaux / année
    output$plt_connaissance_mat_bar <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"CON_MAT")
      .ep_single_hstack(d$longueur_sum, as.character(d$Connaissance),
                        "Matériaux — connaissance")
    })
    output$plt_connaissance_age_bar <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"CON_AGE")
      .ep_single_hstack(d$longueur_sum, as.character(d$Connaissance),
                        "Année de pose — connaissance")
    })

    # donuts TA + précision planimétrique
    output$plt_tranches_age_total <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"AR")
      .ep_donut(d$longueur_sum, as.character(d$tranche_age), "Tranches d'âge (total)", unit_km = TRUE)
    })
    output$plt_precision <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"LIN_CLASS")
      .ep_donut(d[, c("longueur_sum_A","longueur_sum_B","longueur_sum_C")],
                c("Classe A (< 50cm)","Classe B (< 150cm)","Classe C (> 150cm)"),
                "Précision planimétrique", unit_km = TRUE)
    })

    # histogramme matériaux (déjà trié décroissant)
    output$plt_materiaux_bar <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"LRM")
      .ep_bar_materials(d$longueur_sum, as.character(d$materiaupresume), "Familles de matériaux (total)")
    })

    # encarts valeurs droites
    output$cards_valeurs <- renderUI({
      s <- sec(); req(s)
      vpr <- .ep_get(s,"VPR")$Valeur %||% NA
      npp <- .ep_get(s,"NPPTR")$nbre_parcelle %||% NA
      lrs <- .ep_get(s,"LRSPVP")$lineaire %||% NA
      pct <- .ep_get(s,"LRSPVP")$pourcentage %||% NA

      tagList(
        .ep_info_card("Valeur à neuf réseaux", paste0(.ep_fmt_eur(vpr), "\n")),
        .ep_info_card("Parcelles privées traversées",
                      if (is.finite(.ep_num(npp))) paste0(format(.ep_num(npp), big.mark=" ", decimal.mark=",")) else "—"),
        .ep_info_card("Réseaux sur voies privées",
                      paste0(.ep_fmt_km(lrs), " (",
                             if (is.finite(.ep_num(pct))) paste0(round(.ep_num(pct),1), "%") else "—", ")"))
      )
    })

    # -------- Inventaire > Branchements --------

    output$plt_br_ce <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"NBCE")
      values <- c(d$branch_estimes %||% NA, d$branch_connus %||% NA)
      labels <- c("Estimés","Connus")
      .ep_donut(values, labels, "Branchements", unit_km = FALSE)
    })
    output$tbl_br <- renderTable({
      s <- sec(); req(s)
      tot <- .ep_get(s,"NBCE")$branch_tot %||% NA
      vp  <- .ep_get(s,"VP_BR")$VP_branch %||% NA
      tibble::tibble(
        Indicateur = c("Nombre total", "Valeur à neuf (branchements)"),
        Valeur     = c(format(.ep_num(tot), big.mark=" ", decimal.mark=","), .ep_fmt_eur(vp))
      )
    }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 0)

    # -------- Inventaire > Ouvrages --------

    output$tbl_ouvrages <- renderTable({
      s <- sec(); req(s)

      nb_o <- .ep_get(s,"NBEO")   # bassins ouverts/enterrés
      nbt  <- .ep_get(s,"NBEOT")
      nli  <- .ep_get(s,"NLI")
      nno  <- .ep_get(s,"NNO")
      ntr  <- .ep_get(s,"NTR_EP")
      nap  <- .ep_get(s,"NAP")
      npi  <- .ep_get(s,"NPI")
      ndes <- .ep_get(s,"NDES")
      nt   <- .ep_get(s,"NT")     # trémies (STR EP)

      vp_b <- .ep_get(s,"VP_BASSINS")
      vp_ogep <- .ep_get(s,"VP_OGEP")
      vp_ap   <- .ep_get(s,"VP_AP")
      vp_pi   <- .ep_get(s,"VP_PI")
      vp_trem <- .ep_get(s,"VP_TREM")

      # récolements
      r_bass <- .ep_get(s,"NBR")
      r_ap   <- .ep_get(s,"NAPR")
      r_des  <- .ep_get(s,"NDESR")
      r_tr   <- .ep_get(s,"NTR_RECOLES")

      # helpers numériques
      nb_total_bass <- .pickcol(nbt, c("nb_total","total","NBEOT"))
      nb_ouv <- .pickcol(nb_o, c("nb_OUV","ouvert"))
      nb_ent <- .pickcol(nb_o, c("nb_ENT","enterre"))

      li_len <- .pickcol(nli, c("longueur","^NBR_Li$","longueur_sum"))
      noues  <- .pickcol(nno, c("NBR_No","nb","nombre"))
      trench <- .pickcol(ntr, c("NBR_Tr","nb","nombre"))
      n_ap   <- .pickcol(nap, c("NBR_AP","nb","nombre"))
      n_pi   <- .pickcol(npi, c("NBR_PI","nb","nombre"))
      n_des  <- .pickcol(ndes, c("nombre","nb","NBR"))
      n_trem <- .pickcol(nt,   c("nbr_trmie","nb","nombre"))

      vp_b_tot <- .pickcol(vp_b, c("VP_tot","VP_BASSINS","VP_total"))
      vp_b_ouv <- .pickcol(vp_b, c("VP_OUV"))
      vp_b_ent <- .pickcol(vp_b, c("VP_ENT"))

      vp_li   <- .pickcol(vp_ogep, c("VP_OGEP","VAL","Valeur"))
      vp_apv  <- .pickcol(vp_ap,   c("VAL_AP","Valeur"))
      vp_piv  <- .pickcol(vp_pi,   c("VAL_PI","Valeur"))
      vp_trv  <- .pickcol(vp_trem, c("VP_TREM","Valeur"))

      tibble::tibble(
        Ouvrage = c(
          "Bassins",
          "  À ciel ouvert",
          "  Enterrés",
          "Linéaires d'infiltration",
          "  Noues (simples/composées)",
          "  Tranchées (simples/Stockholm)",
          "Arbres de pluie",
          "Puits d'infiltration",
          "Dessableurs",
          "Trémies (STR EP)"
        ),
        Nombre = c(
          .ep_num(nb_total_bass),
          .ep_num(nb_ouv),
          .ep_num(nb_ent),
          .ep_num(li_len),
          .ep_num(noues),
          .ep_num(trench),
          .ep_num(n_ap),
          .ep_num(n_pi),
          .ep_num(n_des),
          .ep_num(n_trem)
        ),
        `Valeur à neuf (€)` = c(
          .ep_fmt_eur(vp_b_tot),
          .ep_fmt_eur(vp_b_ouv),
          .ep_fmt_eur(vp_b_ent),
          .ep_fmt_eur(vp_li),
          "—",
          "—",
          .ep_fmt_eur(vp_apv),
          .ep_fmt_eur(vp_piv),
          "—",
          .ep_fmt_eur(vp_trv)
        ),
        `Récolements` = c(
          .fmt_recol(r_bass),  # bassins
          "—",
          "—",
          "—",                 # LI global
          "—",                 # noues
          "—",                 # tranchées
          .fmt_recol(r_ap),    # AP
          "—",                 # PI
          .fmt_recol(r_des),   # dessableurs
          .fmt_recol(r_tr)     # trémies
        ),
        `Foncier à acquérir` = rep("—", 10)
      )
    }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 0)

    # Donuts d'âge (après tableau)
    output$plt_age_bassins <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"AGE_BASSINS")
      # forme metro : tranche_age | nb_bassins
      vals <- .pickcol(d, c("^nb_bassins","^nbr","^nb$"))
      if (all(is.na(vals))) {
        # fallback (commune : colonnes par tranches)
        vals <- as.numeric(d[1, -1])
        labs <- names(d)[-1]
      } else {
        labs <- as.character(.pickcol(list(texte = d$tranche_age), c("^tranche_age$","^texte$")))
        if (all(is.na(labs))) labs <- as.character(d$tranche_age)
      }
      .ep_donut(vals, labs, "Âge des bassins", unit_km = FALSE)
    })
    output$plt_age_str <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"AGE_STR_EP")
      vals <- .pickcol(d, c("^nb_str","^nbr","^nb$"))
      if (all(is.na(vals))) {
        vals <- as.numeric(d[1, -1]); labs <- names(d)[-1]
      } else {
        labs <- as.character(.pickcol(list(texte = d$tranche_age), c("^tranche_age$","^texte$")))
        if (all(is.na(labs))) labs <- as.character(d$tranche_age)
      }
      .ep_donut(vals, labs, "Âge des STR (EP)", unit_km = FALSE)
    })

    # Réseau électrique (secs) — HT/BT
    output$card_reseau_secs <- renderUI({
      s <- sec(); req(s)
      d <- .ep_get(s,"LRSFA")
      total <- .pickcol(d, c("total","^Total$"))
      ht    <- .pickcol(d, c("longueur_rsec_fort","^HT$","fort"))   # mètres
      bt    <- .pickcol(d, c("longueur_rsec_faible","^BT$","faible"))
      .ep_info_card(
        "RÉSEAU ÉLECTRIQUE (secs)",
        paste0(" - Total : ",        if (is.finite(.ep_num(total))) .ep_fmt_m(total) else "—", "\n"),
        paste0(" - Haute tension : ",if (is.finite(.ep_num(ht)))    .ep_fmt_m(ht)    else "—", "\n"),
        paste0(" - Basse tension : ",if (is.finite(.ep_num(bt)))    .ep_fmt_m(bt)    else "—")
      )
    })

    # Précision planimétrique (secs) — texte
    output$card_precision_secs <- renderUI({
      s <- sec(); req(s)
      d <- .ep_get(s,"LIN_SEC_CLASS")
      a <- .pickcol(d, c("longueur_sum_A","\\bA\\b","classe.*A"))
      b <- .pickcol(d, c("longueur_sum_B","\\bB\\b","classe.*B"))
      c <- .pickcol(d, c("longueur_sum_C","\\bC\\b","classe.*C"))
      tot <- sum(.ep_num(c(a,b,c)), na.rm = TRUE)
      fmt_pct <- function(x) if (tot > 0 && is.finite(.ep_num(x))) paste0(" (", round(.ep_num(x)/tot*100,1), "%)") else ""
      .ep_info_card(
        "Précision planimétrique (réseaux secs)",
        paste0("Classe A (< 50cm) : ", .ep_fmt_km(a), fmt_pct(a), "\n"),
        paste0("Classe B (< 150cm) : ", .ep_fmt_km(b), fmt_pct(b), "\n"),
        paste0("Classe C (> 150cm) : ", .ep_fmt_km(c), fmt_pct(c))
      )
    })

    # -------- État de santé --------

    output$plt_etat_nv <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"ES_NV")
      .ep_donut(d$longueur_sum, d$noteindig, "Collecteurs non visitables — état", unit_km = TRUE)
    })
    output$plt_etat_v <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"ES_V")
      .ep_donut(d$longueur_sum, d$ET_7, "Collecteurs visitables — état", unit_km = TRUE)
    })

    # Encarts texte (ITV / DIAG / PRÉDIAG)
    output$card_litv <- renderUI({
      s <- sec(); req(s)
      d <- .ep_get(s,"LITV")
      .ep_info_card(
        "ITV — linéaire inspecté",
        paste0("Total : ", .ep_fmt_km(.pickcol(d, c("^Total$","^longueur$","^longueur_sum$"))), "\n"),
        paste0("Réalisé dans l'année : ", .ep_fmt_km(.pickcol(d, c("^Realise_dans_lannee$","^annuel$"))), "\n"),
        paste0("Cumulé : ", .ep_fmt_km(.pickcol(d, c("^Total_cumule$","^longueur_cum$"))), "\n"),
        paste0("Pourcentage ITV : ",
               if (is.finite(.ep_num(.pickcol(d, c("^Pourcentage_itv$","^Pourcentage$")))))
                 paste0(round(.ep_num(.pickcol(d, c("^Pourcentage_itv$","^Pourcentage$"))),1), " %") else "—")
      )
    })
    output$card_diag <- renderUI({
      s <- sec(); req(s)
      d <- .ep_get(s,"LDIAG_PREDIAG_DIAG")
      .ep_info_card(
        "DIAG — linéaire inspecté",
        paste0("Total : ", .ep_fmt_km(.pickcol(d, c("^Total$"))), "\n"),
        paste0("Réalisé dans l'année : ", .ep_fmt_km(.pickcol(d, c("^Realise_dans_lannee$"))), "\n"),
        paste0("Cumulé : ", .ep_fmt_km(.pickcol(d, c("^Total_cumule$"))), "\n"),
        paste0("Pourcentage : ",
               if (is.finite(.ep_num(.pickcol(d, c("^Pourcentage$")))))
                 paste0(round(.ep_num(.pickcol(d, c("^Pourcentage$"))),1), " %") else "—")
      )
    })
    output$card_prediag <- renderUI({
      s <- sec(); req(s)
      d <- .ep_get(s,"LDIAG_PREDIAG_PREDIAG")
      .ep_info_card(
        "PRÉDIAG — linéaire inspecté",
        paste0("Total : ", .ep_fmt_km(.pickcol(d, c("^Total$"))), "\n"),
        paste0("Réalisé dans l'année : ", .ep_fmt_km(.pickcol(d, c("^Realise_dans_lannee$"))), "\n"),
        paste0("Cumulé : ", .ep_fmt_km(.pickcol(d, c("^Total_cumule$"))), "\n"),
        paste0("Pourcentage : ",
               if (is.finite(.ep_num(.pickcol(d, c("^Pourcentage$")))))
                 paste0(round(.ep_num(.pickcol(d, c("^Pourcentage$"))),1), " %") else "—")
      )
    })

    # Ouvrages — nombre d'inspections (façon PDF)
    output$tbl_inspections <- renderTable({
      s <- sec(); req(s)
      # Sources pour "Nombre total"
      nb_o <- .ep_get(s,"NBEO")       # bassins ouverts / enterrés
      nli  <- .ep_get(s,"NLI")        # linéaires d'infiltration (km)
      nap  <- .ep_get(s,"NAP")        # arbres de pluie
      npi  <- .ep_get(s,"NPI")        # puits d'infiltration
      ndes <- .ep_get(s,"NDES")       # dessableurs
      nt   <- .ep_get(s,"NT")         # trémies (STR EP)
      ntr  <- .ep_get(s,"NTR_EP")     # tranchées EP

      # Inspections
      lli <- .ep_get(s,"LLI")         # linéaires : longueur / longueur_cum / Pourcentage
      nis <- .ep_get(s,"NI_STR")      # STR : Total / Realise_dans_lannee / Total_cumule / Pourcentage_ins

      nb_bassins <- sum(.ep_num(.pickcol(nb_o, c("nb_ENT"))), .ep_num(.pickcol(nb_o, c("nb_OUV"))), na.rm = TRUE)
      li_len  <- .ep_num(.pickcol(nli, c("^longueur$","^NBR_Li$","^longueur_sum$")))
      ap_nb   <- .ep_num(.pickcol(nap,  c("^NBR_AP$","^nb","^nombre")))
      pi_nb   <- .ep_num(.pickcol(npi,  c("^NBR_PI$","^nb","^nombre")))
      des_nb  <- .ep_num(.pickcol(ndes, c("^nombre$","^nb","^NBR$")))
      trem_nb <- .ep_num(.pickcol(nt,   c("^nbr_trmie$","^nb","^nombre")))
      tr_ep_nb<- .ep_num(.pickcol(ntr,  c("^NBR_Tr$","^nb","^nombre")))

      lli_tot <- .ep_num(.pickcol(lli, c("^longueur$")))
      lli_cum <- .ep_num(.pickcol(lli, c("^longueur_cum$")))
      lli_pct <- .ep_num(.pickcol(lli, c("^Pourcentage$")))

      nis_tot <- .ep_num(.pickcol(nis, c("^Total$")))
      nis_ann <- .ep_num(.pickcol(nis, c("^Realise_dans_lannee$")))
      nis_cum <- .ep_num(.pickcol(nis, c("^Total_cumule$")))
      nis_pct <- .ep_num(.pickcol(nis, c("^Pourcentage_ins$")))

      tibble::tibble(
        Ouvrages = c("Bassins","Linéaires d'infiltration (km)","Arbres de pluie","Puits d'infiltration","Dessableurs","Trémies (STR EP)","Tranchées (EP)","STR"),
        `Nombre total` = c(
          if (is.finite(nb_bassins)) format(nb_bassins, big.mark=" ", decimal.mark=",") else "—",
          if (is.finite(li_len)) .ep_fmt_km(li_len) else "—",
          if (is.finite(ap_nb))  format(ap_nb,  big.mark=" ", decimal.mark=",") else "—",
          if (is.finite(pi_nb))  format(pi_nb,  big.mark=" ", decimal.mark=",") else "—",
          if (is.finite(des_nb)) format(des_nb, big.mark=" ", decimal.mark=",") else "—",
          if (is.finite(trem_nb))format(trem_nb,big.mark=" ", decimal.mark=",") else "—",
          if (is.finite(tr_ep_nb))format(tr_ep_nb,big.mark=" ", decimal.mark=",") else "—",
          if (is.finite(nis_tot)) format(nis_tot, big.mark=" ", decimal.mark=",") else "—"
        ),
        `Inspecté dans l'année` = c(
          "—",
          if (is.finite(lli_tot) && is.finite(lli_cum)) .ep_fmt_km(lli_tot - lli_cum) else "—",
          "—","—","—","—","—",
          if (is.finite(nis_ann)) format(nis_ann, big.mark=" ", decimal.mark=",") else "—"
        ),
        `Total inspecté depuis 1990` = c(
          "—",
          if (is.finite(lli_cum)) .ep_fmt_km(lli_cum) else "—",
          "—","—","—","—","—",
          if (is.finite(nis_cum)) format(nis_cum, big.mark=" ", decimal.mark=",") else "—"
        ),
        `Pourcentage` = c(
          "—",
          if (is.finite(lli_pct)) paste0(round(lli_pct,0), " %") else "—",
          "—","—","—","—","—",
          if (is.finite(nis_pct)) paste0(round(nis_pct,0), " %") else "—"
        ),
      )
    }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 0)

    # Donut : État des STR (EP)
    output$plt_es_str_ep <- renderPlotly({
      s <- sec(); req(s)
      d <- .ep_get(s,"ES_STR_EP")
      if (is.null(d) || !is.data.frame(d)) return(plotly::plotly_empty())
      etat_col <- intersect(names(d), c("Etat","etat","ETAT"))[1]
      n_col    <- intersect(names(d), c("nbr","NBR","nb","Pourcentage"))[1]
      if (is.na(etat_col) || is.na(n_col)) return(plotly::plotly_empty())
      .ep_donut(d[[n_col]], as.character(d[[etat_col]]), "État des STR (EP)", unit_km = FALSE)
    })

    # -------- Programme de travaux --------

    output$card_travaux <- renderUI({
      s <- sec(); req(s)
      lrit <- .ep_get(s,"LRIT")$long_idee %||% NA
      .ep_info_card(
        "Programme de travaux (EP)",
        paste0("Idées de travaux — Réseaux : ", .ep_fmt_km(lrit))
      )
    })
  })
}
