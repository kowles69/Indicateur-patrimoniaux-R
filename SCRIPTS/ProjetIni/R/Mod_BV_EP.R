# R/Mod_BV_EP.R — Bassin versant (EP) : même overlay que Commune

mod_BV_EP_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("tbl_lineaire"), " table{font-size:16.5px; line-height:1.2;}\n",
      "#", ns("tbl_lineaire"), " thead{display:none;}\n",
      "#", ns("tbl_lineaire"), " td,#", ns("tbl_lineaire"), " th{padding:8px 12px;}\n"
    ))),
    uiOutput(ns("crumb"), container = div, class = "eu-crumb"),
    tabsetPanel(
      id = ns("main"), type = "pills",

      tabPanel(
        value = "inv",
        tagList(icon("folder-open")), "Inventaire des patrimoines",
        tabsetPanel(
          id = ns("inv_sub"), type = "pills",
          tabPanel(
            value = "inv_collect",
            tagList(icon("project-diagram")), "Collecteurs",
            br(),
            fluidRow(
              column(3, div(style="min-height:260px;height:260px;overflow:auto;", tableOutput(ns("tbl_lineaire")))),
              column(3, div(style="display:flex;flex-direction:column;height:260px;gap:10px;",
                            plotlyOutput(ns("plt_connaissance_mat_bar"), height="125px"),
                            plotlyOutput(ns("plt_connaissance_age_bar"), height="125px"))),
              column(3, plotlyOutput(ns("plt_tranches_age_total"), height="260px")),
              column(3, plotlyOutput(ns("plt_precision"), height="260px"))
            ),
            fluidRow(
              column(9, plotlyOutput(ns("plt_materiaux_bar"), height="340px")),
              column(3, uiOutput(ns("cards_valeurs"), style="margin-top:12px;"))
            )
          ),
          tabPanel(
            value = "inv_branch",
            tagList(icon("plug")), "Branchements",
            br(),
            fluidRow(
              column(6, plotlyOutput(ns("plt_br_ce"), height="300px")),
              column(6, tableOutput(ns("tbl_br")))
            )
          ),
          tabPanel(
            value = "inv_ouvr",
            tagList(icon("cogs")), "Ouvrages",
            br(),
            tableOutput(ns("tbl_ouvrages")),
            br(),
            fluidRow(
              column(6, uiOutput(ns("card_reseau_secs"))),
              column(6, uiOutput(ns("card_precision_secs")))
            )
          )
        )
      ),

      tabPanel(
        value = "sante",
        tagList(icon("heart-pulse")), "État de santé",
        tabsetPanel(
          id = ns("sante_sub"), type = "pills",
          tabPanel(
            value = "sante_collect",
            tagList(icon("project-diagram")), "Collecteurs",
            br(),
            fluidRow(
              column(6, plotlyOutput(ns("plt_etat_nv"), height="300px")),
              column(6, plotlyOutput(ns("plt_etat_v"),  height="300px"))
            ),
            br(),
            uiOutput(ns("card_itv_diag"))
          ),
          tabPanel(value="sante_branch", tagList(icon("plug")), "Branchements",
                   br(), div(class="muted","Pas de données connues pour l’instant.")),
          tabPanel(value="sante_ouvr", tagList(icon("cogs")), "Ouvrages",
                   br(), tableOutput(ns("tbl_inspections")))
        )
      ),

      tabPanel(value = "travaux", tagList(icon("wrench")), "Programme de travaux",
               br(), uiOutput(ns("card_travaux"))),

      tabPanel(value = "carto", tagList(icon("globe")), "Cartographie",
               br(), div(class="muted","À venir."))
    )
  )
}

mod_BV_EP_server <- function(id, store_list, bassin) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    `%||%` <- function(x,y) if (is.null(x) || length(x)==0) y else x
    num1 <- function(x) suppressWarnings(as.numeric(x))[1]
    fmt_km  <- function(x){ v <- num1(x); if (!is.finite(v)) "—" else paste0(format(round(v,2), big.mark=" ", decimal.mark=",")," km") }
    fmt_m   <- function(x){ v <- num1(x); if (!is.finite(v)) "—" else paste0(format(round(v,0),  big.mark=" ", decimal.mark=",")," m") }
    fmt_eur <- function(x){ v <- num1(x); if (!is.finite(v)) "—" else paste0(format(round(v,0),  big.mark=" ", decimal.mark=",")," €") }

    .to_df <- function(x){
      if (is.data.frame(x)) return(x)
      if (is.null(x)) return(NULL)
      if ("python.builtin.object" %in% class(x)) {
        x <- tryCatch(reticulate::py_to_r(x), error=function(e) NULL)
      }
      if (is.data.frame(x)) x else NULL
    }
    .pick_name <- function(df, cand){
      if (!is.data.frame(df) || !ncol(df)) return(NA_character_)
      for (nm in cand) if (nm %in% names(df)) return(nm)
      for (rx in cand){ ix <- grep(rx, names(df), ignore.case=TRUE, perl=TRUE); if (length(ix)) return(names(df)[ix[1]]) }
      NA_character_
    }
    .get_bv_store_section <- function(store, type_key){
      keys <- c(
        paste0("Bassin versant_", type_key),
        paste0("Bassin versant ", type_key),
        paste0("Bassin_versant_", type_key),
        paste0("BV_", type_key)
      )
      nm <- names(store %||% list())
      for (k in keys){
        if (!is.null(nm) && k %in% nm) return(store[[k]])
        sec <- try(get_section(store, k), silent = TRUE)
        if (!inherits(sec,"try-error") && !is.null(sec)) return(sec)
      }
      NULL
    }
    .bv_get <- function(sec, bassin_name, ...){
      keys <- c(...); if (is.null(sec) || !is.list(sec)) return(NULL)
      obj <- NULL
      for (k in keys) if (!is.null(sec[[k]])) { obj <- sec[[k]]; break }
      if (is.null(obj)) {
        nm <- names(sec);
        for (k in keys){ ix <- grep(k, nm, ignore.case = TRUE, perl = TRUE); if (length(ix)) { obj <- sec[[ nm[ix[1]] ]]; break } }
      }
      d <- .to_df(obj); if (is.null(d) || !nrow(d)) return(NULL)
      col_nom <- .pick_name(d, c("^Bassin$","^Bassin.?versant$","^Nom$","^texte$"))
      if (!is.na(col_nom)) {
        clean <- function(s) tolower(trimws(enc2utf8(as.character(s))))
        d <- d[ clean(d[[col_nom]]) == clean(bassin_name), , drop = FALSE]
      }
      d
    }

    sec <- reactive({
      req(store_list, bassin)
      .get_bv_store_section(if (is.function(store_list)) store_list() else store_list, "EP")
    })

    output$crumb <- renderUI({
      tagList(
        span(class="crumb-main", paste0("Bassin versant : ", bassin())),
        span(class="crumb-sep", icon("angle-right")),
        span(class="crumb-sub",
             switch(input$main %||% "inv",
                    inv="Inventaire", sante="État de santé",
                    travaux="Programme de travaux", carto="Cartographie"))
      )
    })

    # ---------- Inventaire > Collecteurs ----------
    output$tbl_lineaire <- renderTable({
      s <- sec(); req(s)
      lep <- .bv_get(s, bassin(), "LEP")
      lrv <- .bv_get(s, bassin(), "LRV")
      tibble::tibble(
        `Libellé` = c("Total EP", "Réseaux visitables (>=1,5m)", "Réseaux non visitables"),
        `Valeur`  = c(
          fmt_km(lep$longueur_sum %||% lep$Total %||% lep$total %||% NA),
          fmt_km(lrv$longueur_visitable %||% NA),
          fmt_km(lrv$longueur_non_visitable %||% NA)
        )
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=2)

    read_connaissance_vec <- function(d){
      if (is.null(d) || !nrow(d)) return(list(values=NA, labels="Aucune donnée"))
      nm <- names(d)
      pick <- function(pats){ for (p in pats){ ix <- grep(p, nm, ignore.case=TRUE); if (length(ix)) return(nm[ix[1]]) }; NA_character_ }
      vals <- c(
        num1(d[1, pick(c("^%?\\s*conn","connus?")), drop=TRUE]),
        num1(d[1, pick(c("^%?\\s*estim","estim[ée]s?")), drop=TRUE]),
        num1(d[1, pick(c("pas.*don","sans.*don")), drop=TRUE])
      )
      labs <- c("Connus","Estimés","Pas de données")
      ok <- is.finite(vals) & vals>=0
      list(values=vals[ok], labels=labs[ok])
    }
    read_mat_large <- function(d){
      if (is.null(d) || !nrow(d)) return(list(values=0, labels="Aucune donnée"))
      nm <- setdiff(names(d), c("texte","Total","total","TOTAL","Bassin","Nom"))
      vals <- suppressWarnings(as.numeric(d[1, nm, drop=TRUE]))
      ok <- is.finite(vals) & vals>=0
      list(values=vals[ok], labels=nm[ok])
    }

    output$plt_connaissance_mat_bar <- renderPlotly({
      cc <- read_connaissance_vec(.bv_get(sec(), bassin(), "CON_MAT","ConMat"))
      .eu_single_hstack(cc$values, cc$labels, "Matériaux — connaissance")
    })
    output$plt_connaissance_age_bar <- renderPlotly({
      cc <- read_connaissance_vec(.bv_get(sec(), bassin(), "CON_AGE","ConAge"))
      .eu_single_hstack(cc$values, cc$labels, "Année de pose — connaissance")
    })

    output$plt_tranches_age_total <- renderPlotly({
      d <- .bv_get(sec(), bassin(), "AR"); req(d, nrow(d)>=1)
      labs <- setdiff(names(d), c("texte","Total","total","Bassin","Nom"))
      vals <- suppressWarnings(as.numeric(d[1, labs, drop=TRUE]))
      .eu_donut(vals, labs, "Tranches d'âge (total)")
    })

    output$plt_precision <- renderPlotly({
      d <- .bv_get(sec(), bassin(), "LIN_CLASS","LinClass")
      if (is.null(d)) return(.eu_donut(c(1), c("Aucune donnée"), "Précision planimétrique"))
      .eu_donut(d[, c("longueur_sum_A","longueur_sum_B","longueur_sum_C")],
                c("Classe A (< 50cm)","Classe B (< 150cm)","Classe C (> 150cm)"),
                "Précision planimétrique")
    })

    output$plt_materiaux_bar <- renderPlotly({
      mm <- read_mat_large(.bv_get(sec(), bassin(), "LRM"))
      .eu_bar_materials(mm$values, mm$labels, "Familles de matériaux (total)")
    })

    output$cards_valeurs <- renderUI({
      s <- sec(); req(s)
      vpr <- (.bv_get(s, bassin(), "VPR")$Valeur)       %||% NA
      npp <- (.bv_get(s, bassin(), "NPPTR")$nbre_parcelle) %||% NA
      lrs <- (.bv_get(s, bassin(), "LRSPVP")$lineaire)  %||% NA
      pct <- (.bv_get(s, bassin(), "LRSPVP")$pourcentage)%||% NA
      tagList(
        .eu_info_card("Valeur à neuf réseaux", paste0(fmt_eur(vpr), "\n")),
        .eu_info_card("Parcelles privées traversées",
                      if (is.finite(num1(npp))) format(num1(npp), big.mark=" ", decimal.mark=",") else "—"),
        .eu_info_card("Réseaux sur voies privées",
                      paste0(fmt_km(lrs), " (", if (is.finite(num1(pct))) paste0(round(num1(pct),1), "%") else "—", ")"))
      )
    })

    # ---------- Inventaire > Branchements ----------
    output$plt_br_ce <- renderPlotly({
      d <- .bv_get(sec(), bassin(), "NBCE")
      .eu_donut(c(d$branch_estimes %||% NA, d$branch_connus %||% NA),
                c("Estimés","Connus"), "Branchements", unit_km = FALSE)
    })
    output$tbl_br <- renderTable({
      s <- sec(); req(s)
      tibble::tibble(
        Indicateur = c("Nombre total", "Valeur à neuf (branchements)"),
        Valeur     = c(format(num1(.bv_get(s, bassin(), "NBCE")$branch_tot), big.mark=" ", decimal.mark=","),
                       fmt_eur(.bv_get(s, bassin(), "VP_BR","VPBr")$VP_branch %||% NA))
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=0)

    # ---------- Inventaire > Ouvrages ----------
    output$tbl_ouvrages <- renderTable({
      s <- sec(); req(s)
      nap <- .bv_get(s,bassin(),"NAP")$NBR_AP  %||% NA
      npi <- .bv_get(s,bassin(),"NPI")$NBR_PI  %||% NA
      nno <- .bv_get(s,bassin(),"NNO")$NBR_No  %||% NA
      ntr <- .bv_get(s,bassin(),"NT") $nbr_trmie%||% NA
      vpap<- .bv_get(s,bassin(),"VP_AP")$VAL_AP %||% NA
      vppi<- .bv_get(s,bassin(),"VP_PI")$VAL_PI %||% NA
      vpog<- .bv_get(s,bassin(),"VP_OGEP")$VP_OGEP %||% NA
      vptr<- .bv_get(s,bassin(),"VP_TREM")$VP_TREM %||% NA
      tibble::tibble(
        Ouvrage = c("Avaloirs (AP)", "Puisards (PI)", "Ouvrages de gestion (OGEP)", "Trémies"),
        Nombre  = c(num1(nap), num1(npi), num1(nno), num1(ntr)),
        `Valeur à neuf (€)` = c(fmt_eur(vpap), fmt_eur(vppi), fmt_eur(vpog), fmt_eur(vptr)),
        `Récolements` = c("—","—","—","—"),
        `Foncier à acquérir` = c("—","—","—","—")
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=0)

    output$card_reseau_secs <- renderUI({
      d <- .bv_get(sec(), bassin(), "LRSFA")
      .eu_info_card(
        "RÉSEAU ÉLECTRIQUE (secs)",
        paste0(" - Total : ",        fmt_m(d$total[1]                 %||% NA), "\n"),
        paste0(" - Haute tension : ",fmt_m(d$longueur_rsec_fort[1]    %||% NA), "\n"),
        paste0(" - Basse tension : ",fmt_m(d$longueur_rsec_faible[1]  %||% NA))
      )
    })
    output$card_precision_secs <- renderUI({
      d <- .bv_get(sec(), bassin(), "LIN_SEC_CLASS")
      a <- d$longueur_sum_A[1] %||% NA; b <- d$longueur_sum_B[1] %||% NA; c <- d$longueur_sum_C[1] %||% NA
      tot <- sum(c(num1(a),num1(b),num1(c)), na.rm = TRUE)
      pct <- function(x) if (is.finite(num1(x)) && tot>0) paste0(" (", round(num1(x)/tot*100,1), "%)") else ""
      .eu_info_card("Précision planimétrique (réseaux secs)",
                    paste0("Classe A (< 50cm) : ", fmt_km(a), pct(a), "\n"),
                    paste0("Classe B (< 150cm) : ", fmt_km(b), pct(b), "\n"),
                    paste0("Classe C (> 150cm) : ", fmt_km(c), pct(c)))
    })

    # ---------- État de santé ----------
    build_etat <- function(d){
      if (is.null(d) || !nrow(d)) return(list(values=NA,labels="Aucune donnée"))
      nm <- names(d); num <- function(x) suppressWarnings(as.numeric(x))[1]
      pick <- function(...){ pats <- c(...); for (p in pats){ ix <- grep(p,nm,ignore.case=TRUE,perl=TRUE); if (length(ix)) return(nm[ix[1]]) } ; NA_character_}
      cols <- c(
        bon  = pick("^.?[eé]tat.?\\s*bon$"), corr = pick("^.?[eé]tat.?\\s*cor"),
        degr = pick("^.?[eé]tat.?\\s*d[ée]gr"), mauv = pick("^.?[eé]tat.?\\s*mau"),
        itv  = pick("itv.*incompl"), pdd = pick("pas.*don")
      )
      labs <- c("État bon","État correct","État dégradé","État mauvais","ITV incomplètes","Pas de données")
      vals <- c(num(d[1, cols["bon"],drop=TRUE]), num(d[1, cols["corr"],drop=TRUE]),
                num(d[1, cols["degr"],drop=TRUE]), num(d[1, cols["mauv"],drop=TRUE]),
                num(d[1, cols["itv"], drop=TRUE]), num(d[1, cols["pdd"], drop=TRUE]))
      ok <- is.finite(vals) & vals>=0; list(values=vals[ok], labels=labs[ok])
    }
    output$plt_etat_nv <- renderPlotly({
      ee <- build_etat(.bv_get(sec(), bassin(), "ES_NV"))
      .eu_donut(ee$values, ee$labels, "Collecteurs non visitables — état")
    })
    output$plt_etat_v <- renderPlotly({
      ee <- build_etat(.bv_get(sec(), bassin(), "ES_V"))
      .eu_donut(ee$values, ee$labels, "Collecteurs visitables — état")
    })

    output$card_itv_diag <- renderUI({
      s <- sec(); req(s)
      litv <- .bv_get(s,bassin(),"LITV")
      diag <- .bv_get(s,bassin(),"LDIAG_PREDIAG_DIAG")
      pre  <- .bv_get(s,bassin(),"LDIAG_PREDIAG_PREDIAG")
      .eu_info_card("COLLECTEURS : linéaire inspecté",
                    paste0("Réseaux non visitables — ITV (km) : ", fmt_km(litv$Total %||% NA), "\n"),
                    paste0("Réseaux visitables — ITV dans l'année : ", fmt_km(litv$Realise_dans_lannee %||% NA), "\n"),
                    if (!is.null(diag)) paste0("Diagnostics (km) : ", fmt_km(diag$Total %||% NA), "\n") else "",
                    if (!is.null(pre))  paste0("Prédiagnostics (km) : ", fmt_km(pre$Total  %||% NA)) else "")
    })

    # ---------- Travaux ----------
    output$tbl_inspections <- renderTable({ tibble::tibble(Information="—") })
    output$card_travaux <- renderUI({
      lrit <- .bv_get(sec(), bassin(),"LRIT")$long_idee %||% NA
      .eu_info_card("Programme de travaux (EP)",
                    paste0("Idées de travaux — Réseaux : ", fmt_km(lrit)))
    })

  })
}




