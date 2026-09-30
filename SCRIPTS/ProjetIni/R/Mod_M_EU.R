# R/Mod_M_EU.R — Module Métropole — EU (corrigé v6)
# - Harmonisation visuelle (marges, styles) sur tout l’onglet Ouvrages
# - RÉSEAU ÉLECTRIQUE (secs) : extraction HT/BT/TOTAL robuste depuis LRSFa
#   (noms différents, format colonnes OU format long libellé/valeur)
# - Conserve toutes les améliorations des versions précédentes

library(dplyr)
library(plotly)
library(magrittr)

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------- utils ----------------

.eu_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  x_chr <- as.character(x)
  x_chr <- gsub("[^0-9,.-]", "", x_chr)
  x_chr <- gsub(",", ".", x_chr, fixed = TRUE)
  suppressWarnings(as.numeric(x_chr))
}

.eu_fmt_km <- function(x) {
  v <- .eu_num(x)
  if (!is.finite(v)) return("—")
  paste0(format(round(v, 2), big.mark = " ", decimal.mark = ","), " km")
}

.eu_fmt_m <- function(x) {
  v <- .eu_num(x)
  if (!is.finite(v)) return("—")
  paste0(format(round(v, 0), big.mark = " ", decimal.mark = ","), " m")
}

.eu_fmt_eur <- function(x) {
  v <- .eu_num(x)
  if (!is.finite(v)) return("—")
  paste0(format(round(v, 0), big.mark = " ", decimal.mark = ","), " €")
}

.eu_info_card <- function(title, ..., class = "") {
  div(class = paste("eu-infocard", class),
      div(class = "eu-infocard-title", title),
      pre(class  = "eu-infocard-body", paste0(..., collapse = "")))
}

# récupère une colonne (par nom/regex) ou une ligne (format long libellé/valeur)
.pickcol <- function(df, choices_regex) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NA_real_)
  nms <- names(df)
  # exact
  for (c in choices_regex) {
    idx <- which(tolower(nms) == tolower(c))
    if (length(idx)) return(df[[idx[1]]])
  }
  # regex
  for (c in choices_regex) {
    idx <- grep(c, nms, ignore.case = TRUE)
    if (length(idx)) return(df[[idx[1]]])
  }
  # format long : 1re col libellé, 2e col valeur
  if (ncol(df) >= 2) {
    lib <- as.character(df[[1]])
    for (c in choices_regex) {
      rows <- grep(c, lib, ignore.case = TRUE)
      if (length(rows)) return(df[[2]][rows[1]])
    }
  }
  NA_real_
}

# ---- thème plotly commun (alignement parfait entre graphes) ----
.eu_plot_theme <- function() {
  list(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    margin = list(l = 6, r = 6, t = 35, b = 0)   # top assez grand pour ne pas couper les titres
  )
}

# Donut avec % + valeur (km par défaut)
.eu_donut <- function(values, labels, title = "", min_pct_label = 0.045, unit_km = TRUE) {
  values <- .eu_num(values)
  ok <- is.finite(values) & values >= 0
  values <- values[ok]; labels <- labels[ok]
  if (length(values) == 0 || sum(values) == 0) { values <- 1; labels <- "Aucune donnée" }

  tot <- sum(values)
  pct <- values / tot
  value_txt <- if (unit_km)
    paste0(format(round(values, 2), big.mark = " ", decimal.mark = ","), " km")
  else
    format(round(values, 0), big.mark = " ", decimal.mark = ",")
  txt <- ifelse(pct >= min_pct_label, paste0(round(pct * 100, 1), "%<br>", value_txt), "")

  thm <- .eu_plot_theme()

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
      title  = list(text = title, font = list(size = 14, family = "Arial Black")),
      legend = list(orientation = "v"),
      paper_bgcolor = thm$paper_bgcolor,
      plot_bgcolor  = thm$plot_bgcolor,
      margin = thm$margin
    )
}

# Barres empilées horizontales (1 barre) — mêmes marges/fonds que donuts
.eu_single_hstack <- function(values, labels, title = "") {
  values <- .eu_num(values)
  ok <- is.finite(values) & values >= 0
  values <- values[ok]; labels <- labels[ok]
  if (length(values) == 0 || sum(values) == 0) { values <- 1; labels <- "Aucune donnée" }
  tot <- sum(values)
  pct <- values / tot

  thm <- .eu_plot_theme()

  p <- plot_ly(orientation = "h")
  for (i in seq_along(values)) {
    p <- add_trace(
      p, type = "bar", x = values[i], y = "", name = labels[i],
      text = paste0(round(pct[i] * 100, 1), "% — ",
                    format(round(values[i], 2), big.mark = " ", decimal.mark = ","), " km"),
      textposition = "inside", texttemplate = "%{text}",
      hovertemplate = "%{fullData.name}<br>%{x} km (%{percent})<extra></extra>",
      marker = list(line = list(color = "white", width = 1)), showlegend = TRUE
    )
  }
  p %>% layout(
    barmode = "stack",
    title = list(text = title, font = list(size = 14, family = "Arial Black")),
    xaxis = list(title = NULL),
    yaxis = list(title = NULL, showticklabels = FALSE),
    paper_bgcolor = thm$paper_bgcolor,
    plot_bgcolor  = thm$plot_bgcolor,
    margin = thm$margin
  )
}

# Histogramme horizontal (matériaux) — tri décroissant (haut -> bas)
.eu_bar_materials <- function(values, labels, title = "") {
  values <- .eu_num(values)
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

  thm <- .eu_plot_theme()

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

.eu_get <- function(sec, key) {
  obj <- sec[[key]]
  if (is.null(obj)) return(NULL)
  as.data.frame(obj)
}

# Icône Collecteurs (FA5 stable)
.fa_pipe_icon <- function() icon("project-diagram")  # ou icon("stream")

# ------ Extraction robuste LRSFa (HT/BT/TOTAL, mètres) ------
.eu_lrsfa_extract <- function(df) {
  out <- list(total = NA_real_, ht = NA_real_, bt = NA_real_)
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(out)

  # motifs élargis + noms exacts détectés chez toi
  pat_total <- c("^total$", "total", "longueur[[:space:]]*totale", ".*total.*")
  pat_ht    <- c("^ht$", "haute[[:space:]]*tension", "courants?[[:space:]]*forts?",
                 "courant[[:space:]]*fort", "^cfo$", "c[.]?f[.]?o?",
                 "longueur_rsec_fort", "^.*rsec.*fort$", "longueur.*fort")
  pat_bt    <- c("^bt$", "basse[[:space:]]*tension", "courants?[[:space:]]*faibles?",
                 "courant[[:space:]]*faible", "^cfa$", "c[.]?fa(i)?b(le)?s?",
                 "longueur_rsec_faible", "^.*rsec.*faible$", "longueur.*faible")

  pick_by_colname <- function(df, pats) {
    nms <- names(df)
    for (p in pats) {
      idx <- which(tolower(nms) == tolower(p))
      if (length(idx)) return(df[[idx[1]]])
    }
    for (p in pats) {
      idx <- grep(p, nms, ignore.case = TRUE)
      if (length(idx)) return(df[[idx[1]]])
    }
    NA_real_
  }
  pick_by_label <- function(df, pats) {
    if (ncol(df) < 2) return(NA_real_)
    lib <- as.character(df[[1]])
    val <- df[[2]]
    for (p in pats) {
      idx <- grep(p, lib, ignore.case = TRUE)
      if (length(idx)) return(val[[idx[1]]])
    }
    NA_real_
  }

  total <- pick_by_colname(df, pat_total)
  ht    <- pick_by_colname(df, pat_ht)
  bt    <- pick_by_colname(df, pat_bt)

  if (!is.finite(.eu_num(total))) total <- pick_by_label(df, pat_total)
  if (!is.finite(.eu_num(ht)))    ht    <- pick_by_label(df, pat_ht)
  if (!is.finite(.eu_num(bt)))    bt    <- pick_by_label(df, pat_bt)

  list(total = .eu_num(total), ht = .eu_num(ht), bt = .eu_num(bt))
}


# ---------------- UI ----------------

mod_M_EU_ui <- function(id, title = "Métropole — EU") {
  ns <- NS(id)

  tagList(
    # Styles locaux (lisibilité du tableau Collecteurs)
    tags$style(HTML(paste0(
      "#", ns("tbl_lineaire"), " table{font-size:16.5px; line-height:1.2;}\n",
      "#", ns("tbl_lineaire"), " thead{display:none;}\n",
      "#", ns("tbl_lineaire"), " td,#", ns("tbl_lineaire"), " th{padding:8px 12px;}\n"
    ))),

    # Fil d’ariane dynamique
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

          # Collecteurs — 2 lignes
          tabPanel(
            value = "inv_collect",
            tagList(.fa_pipe_icon()), "Collecteurs",
            br(),
            # LIGNE 1 — 4 blocs à même hauteur (260px)
            fluidRow(
              column(3,
                     div(style = "min-height:260px;height:260px;overflow:auto;",
                         tableOutput(ns("tbl_lineaire")))
              ),
              column(3,
                     div(style="display:flex;flex-direction:column;height:260px;gap:10px;",
                         plotlyOutput(ns("plt_connaissance_mat_bar"), height = "125px"),
                         plotlyOutput(ns("plt_connaissance_age_bar"), height = "125px")
                     )
              ),
              column(3, plotlyOutput(ns("plt_tranches_age_total"), height = "260px")),
              column(3, plotlyOutput(ns("plt_precision"),          height = "260px"))
            ),
            # LIGNE 2
            fluidRow(
              column(9, plotlyOutput(ns("plt_materiaux_bar"), height = "340px")),
              column(3, uiOutput(ns("cards_valeurs"), style="margin-top:12px;"))
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

          # Ouvrages (inclut RÉSEAUX SECS)
          tabPanel(
            value = "inv_ouvr",
            tagList(icon("cogs")), "Ouvrages",
            br(),
            tableOutput(ns("tbl_ouvrages")),
            br(),
            fluidRow(
              column(6, uiOutput(ns("card_reseau_secs"))),
              column(6, uiOutput(ns("card_precision_secs")))
            ),
            br(),
            fluidRow(
              column(6, plotlyOutput(ns("plt_age_steu"), height = "260px")),
              column(6, plotlyOutput(ns("plt_age_str"),  height = "260px"))
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
            tagList(.fa_pipe_icon()), "Collecteurs",
            br(),
            fluidRow(
              column(6, plotlyOutput(ns("plt_etat_nv"), height = "300px")),
              column(6, plotlyOutput(ns("plt_etat_v"),  height = "300px"))
            ),
            br(),
            uiOutput(ns("card_itv_diag"))
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
            fluidRow(
              column(6, plotlyOutput(ns("plt_etat_steu"), height = "300px")),
              column(6, plotlyOutput(ns("plt_etat_str"),  height = "300px"))
            )
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

mod_M_EU_server <- function(id, store_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sec <- reactive({
      req(store_list)
      store_list[["Metropole_EU_U"]]
    })

    # fil d’ariane dynamique
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
      tibble::tibble(
        `Libellé` = c(
          "Réseaux unitaires (U)",
          "Réseaux eaux usées (EU)",
          "Total EU/U",
          "Réseaux visitables (>=1,5m)",
          "Réseaux non visitables"
        ),
        `Valeur` = c(
          .eu_fmt_km(.eu_get(s,"LRU")[[1]]),
          .eu_fmt_km(.eu_get(s,"LREU")[[1]]),
          .eu_fmt_km(.eu_get(s,"LEP")[[1]]),
          .eu_fmt_km(.eu_get(s,"LRV")$longueur_visitable),
          .eu_fmt_km(.eu_get(s,"LRV")$longueur_non_visitable)
        )
      )
    }, striped = TRUE, hover = TRUE, spacing = 'xs', digits = 2)

    # barres empilées : connaissance matériaux / année
    output$plt_connaissance_mat_bar <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"ConMat")
      .eu_single_hstack(d$longueur_sum, as.character(d$Connaissance),
                        "Matériaux — connaissance")
    })
    output$plt_connaissance_age_bar <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"ConAge")
      .eu_single_hstack(d$longueur_sum, as.character(d$Connaissance),
                        "Année de pose — connaissance")
    })

    # donuts : TA + précision
    output$plt_tranches_age_total <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"AR")
      .eu_donut(d$longueur_sum, as.character(d$tranche_age),
                "Tranches d'âge (total)")
    })
    output$plt_precision <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"LinClass")
      .eu_donut(d[, c("longueur_sum_A","longueur_sum_B","longueur_sum_C")],
                c("Classe A (< 50cm)","Classe B (< 150cm)","Classe C (> 150cm)"),
                "Précision planimétrique")
    })

    # grand histogramme matériaux
    output$plt_materiaux_bar <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"LRM")
      .eu_bar_materials(d$longueur_sum, as.character(d$materiaupresume),
                        "Familles de matériaux (total)")
    })

    # encarts valeurs (à droite en ligne 2)
    output$cards_valeurs <- renderUI({
      s <- sec(); req(s)
      vpr <- .eu_get(s,"VPR")$Valeur %||% NA
      npp <- .eu_get(s,"NPPTR")$nbre_parcelle %||% NA
      lrs <- .eu_get(s,"LRSPVP")$lineaire %||% NA
      pct <- .eu_get(s,"LRSPVP")$pourcentage %||% NA

      tagList(
        .eu_info_card("Valeur à neuf réseaux", paste0(.eu_fmt_eur(vpr), "\n")),
        .eu_info_card("Parcelles privées traversées",
                      if (is.finite(.eu_num(npp))) paste0(format(.eu_num(npp), big.mark=" ", decimal.mark=",")) else "—"),
        .eu_info_card("Réseaux sur voies privées",
                      paste0(.eu_fmt_km(lrs), " (",
                             if (is.finite(.eu_num(pct))) paste0(round(.eu_num(pct),1), "%") else "—", ")"))
      )
    })

    # -------- Inventaire > Branchements --------

    output$plt_br_ce <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"NBCE")
      values <- c(d$branch_estimes %||% NA, d$branch_connus %||% NA)
      labels <- c("Estimés","Connus")
      .eu_donut(values, labels, "Branchements", unit_km = FALSE)
    })

    output$tbl_br <- renderTable({
      s <- sec(); req(s)
      tot <- .eu_get(s,"NBCE")$branch_tot %||% NA
      vp  <- .eu_get(s,"VPBr")$VP_branch %||% NA
      tibble::tibble(
        Indicateur = c("Nombre total", "Valeur à neuf (branchements)"),
        Valeur     = c(format(.eu_num(tot), big.mark=" ", decimal.mark=","), .eu_fmt_eur(vp))
      )
    }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 0)

    # -------- Inventaire > Ouvrages (inclut RÉSEAUX SECS) --------

    output$tbl_ouvrages <- renderTable({
      s <- sec(); req(s)
      nsteu <- .eu_get(s,"NSTEU")[[1]] %||% NA
      nstr  <- .eu_get(s,"NSTR") [[1]] %||% NA
      nfpr  <- .eu_get(s,"NFPR") [[1]] %||% NA
      ndo   <- .eu_get(s,"NDO")  [[1]] %||% NA     # Déversoirs d'orage
      ndes  <- .eu_get(s,"NDes") [[1]] %||% NA     # Dessableurs
      vpsteu<- .eu_get(s,"VPSTEU")[[1]] %||% NA
      vpstr <- .eu_get(s,"VPSTR") [[1]] %||% NA
      vpfpr <- .eu_get(s,"VPFPR") [[1]] %||% NA
      vpdo  <- (.eu_get(s,"VPDO") [[1]] %||% NA)   # si dispo
      vpdes <- (.eu_get(s,"VPDES")[[1]] %||% NA)   # si dispo

      tibble::tibble(
        Ouvrage = c("STEU","STR","FPR","Déversoirs d'orage","Dessableurs"),
        Nombre  = c(.eu_num(nsteu), .eu_num(nstr), .eu_num(nfpr), .eu_num(ndo), .eu_num(ndes)),
        `Valeur à neuf (€)` = c(.eu_fmt_eur(vpsteu), .eu_fmt_eur(vpstr), .eu_fmt_eur(vpfpr),
                                .eu_fmt_eur(vpdo), .eu_fmt_eur(vpdes)),
        `Récolements` = c("—","—","—","—","—"),
        `Foncier à acquérir` = c("—","—","—","—","—")
      )
    }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 0)

    # Réseau électrique (secs) — HT/BT depuis LRSFa (mètres)
    output$card_reseau_secs <- renderUI({
      s <- sec(); req(s)
      d <- .eu_get(s,"LRSFa")
      ext <- .eu_lrsfa_extract(d)
      .eu_info_card(
        "RÉSEAU ÉLECTRIQUE (secs)",
        paste0(" - Total : ",        .eu_fmt_m(ext$total), "\n"),
        paste0(" - Haute tension : ", .eu_fmt_m(ext$ht),    "\n"),
        paste0(" - Basse tension : ", .eu_fmt_m(ext$bt))
      )
    })

    # Précision planimétrique (réseaux secs) — texte
    output$card_precision_secs <- renderUI({
      s <- sec(); req(s)
      d <- .eu_get(s,"LIN_SEC_CLASS")
      a <- .pickcol(d, c("longueur_sum_A","\\bA\\b","classe.*A"))
      b <- .pickcol(d, c("longueur_sum_B","\\bB\\b","classe.*B"))
      c <- .pickcol(d, c("longueur_sum_C","\\bC\\b","classe.*C"))
      tot <- sum(.eu_num(c(a,b,c)), na.rm = TRUE)
      fmt_pct <- function(x) if (tot > 0 && is.finite(.eu_num(x))) paste0(" (", round(.eu_num(x)/tot*100,1), "%)") else ""
      .eu_info_card(
        "Précision planimétrique (réseaux secs)",
        paste0("Classe A (< 50cm) : ", .eu_fmt_km(a), fmt_pct(a), "\n"),
        paste0("Classe B (< 150cm) : ", .eu_fmt_km(b), fmt_pct(b), "\n"),
        paste0("Classe C (> 150cm) : ", .eu_fmt_km(c), fmt_pct(c))
      )
    })

    # -------- État de santé --------

    output$plt_etat_nv <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"ES_NV")
      .eu_donut(d$longueur_sum, d$noteindig, "Collecteurs non visitables — état")
    })

    output$plt_etat_v <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"ES_V")
      .eu_donut(d$longueur_sum, d$ET_7, "Collecteurs visitables — état")
    })

    output$card_itv_diag <- renderUI({
      s <- sec(); req(s)
      litv <- .eu_get(s,"LITV")
      diag <- .eu_get(s,"LDIAG_PREDIAG_DIAG")
      pre  <- .eu_get(s,"LDIAG_PREDIAG_PREDIAG")
      .eu_info_card(
        "COLLECTEURS : linéaire inspecté",
        paste0("Réseaux non visitables — ITV (km) : ", .eu_fmt_km(litv$Total), "\n"),
        paste0("Réseaux visitables — ITV dans l'année : ", .eu_fmt_km(litv$Realise_dans_lannee), "\n"),
        if (!is.null(diag)) paste0("Diagnostics (km) : ", .eu_fmt_km(diag$Total), "\n") else "",
        if (!is.null(pre))  paste0("Prédiagnostics (km) : ", .eu_fmt_km(pre$Total)) else ""
      )
    })

    output$tbl_inspections <- renderTable({
      s <- sec(); req(s)
      steu <- .eu_get(s,"NISTEU")
      str  <- .eu_get(s,"NISTR")
      tibble::tibble(
        Ouvrage = c("STEU", "STR"),
        `Nombre total` = c(.eu_num(steu$Total), .eu_num(str$Total)),
        `Inspecté dans l'année` = c(.eu_num(steu$Realise_dans_lannee), .eu_num(str$Realise_dans_lannee)),
        `Total inspecté depuis 1990` = c(.eu_num(steu$Total_cumule), .eu_num(str$Total_cumule)),
        `Pourcentage` = c(.eu_num(steu$Pourcentage_ins), .eu_num(str$Pourcentage_ins))
      )
    }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 1)

    output$plt_etat_steu <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"ES_STEU")
      .eu_donut(d$nbr, d$Etat, "STEU — état de santé", unit_km = FALSE)
    })

    output$plt_etat_str <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s,"ES_STR")
      .eu_donut(d$nbr, d$Etat, "STR — état de santé", unit_km = FALSE)
    })

    # -------- Programme de travaux --------

    output$card_travaux <- renderUI({
      s <- sec(); req(s)
      lrit <- .eu_get(s,"LRIT")$long_idee %||% NA
      .eu_info_card(
        "Programme de travaux (EU)",
        paste0("Idées de travaux — Réseaux : ", .eu_fmt_km(lrit))
      )
    })

    # -------- Ouvrages : Tranches d'âge STEU & STR --------

    output$plt_age_steu <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s, "AGESTEU")
      if (is.null(d)) return(NULL)
      .eu_donut(d$longueur_sum, as.character(d$tranche_age), "Tranches d'âge — STEU")
    })

    output$plt_age_str <- renderPlotly({
      s <- sec(); req(s)
      d <- .eu_get(s, "AGESTR")
      if (is.null(d)) return(NULL)
      .eu_donut(d$longueur_sum, as.character(d$tranche_age), "Tranches d'âge — STR")
    })
  })
}
