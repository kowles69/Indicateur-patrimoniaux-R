# R/Mod_C_EP.R — Module Commune — EP (disposition identique à C_EU)

mod_C_EP_ui <- function(id) {
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

      # INVENTAIRE
      tabPanel(
        value = "inv",
        tagList(icon("folder-open")), "Inventaire des patrimoines",

        tabsetPanel(
          id = ns("inv_sub"), type = "pills",

          # Collecteurs
          tabPanel(
            value = "inv_collect",
            tagList(icon("project-diagram")), "Collecteurs",
            br(),
            fluidRow(
              column(3, div(style="min-height:260px;height:260px;overflow:auto;", tableOutput(ns("tbl_lineaire")))),
              column(3, div(style="display:flex;flex-direction:column;height:260px;gap:10px;",
                            plotlyOutput(ns("plt_connaissance_mat_bar"), height = "125px"),
                            plotlyOutput(ns("plt_connaissance_age_bar"), height = "125px"))),
              column(3, plotlyOutput(ns("plt_tranches_age_total"), height = "260px")),
              column(3, plotlyOutput(ns("plt_precision"), height = "260px"))
            ),
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
            )
          )
        )
      ),

      # ÉTAT DE SANTÉ
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
          tabPanel(
            value="sante_branch", tagList(icon("plug")), "Branchements",
            br(), div(class="muted","Pas de données connues pour l’instant.")
          ),
          tabPanel(
            value="sante_ouvr", tagList(icon("cogs")), "Ouvrages",
            br(), tableOutput(ns("tbl_inspections"))
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

      # CARTOGRAPHIE
      tabPanel(
        value = "carto",
        tagList(icon("globe")), "Cartographie",
        br(),
        div(class="muted","À venir.")
      )
    )
  )
}

mod_C_EP_server <- function(id, store_list, commune) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # section commune EP (filtrée par utils via colonne 'texte')
    sec <- reactive({
      req(store_list, commune)
      get_commune_section(store_list, commune(), type="EP")
    })

    num1 <- function(x) suppressWarnings(as.numeric(x))[1]
    fmt_km  <- function(x) { v <- num1(x); if (!is.finite(v)) "—" else paste0(format(round(v,2), big.mark=" ", decimal.mark=","), " km") }
    fmt_m   <- function(x) { v <- num1(x); if (!is.finite(v)) "—" else paste0(format(round(v,0),  big.mark=" ", decimal.mark=","), " m") }
    fmt_eur <- function(x) { v <- num1(x); if (!is.finite(v)) "—" else paste0(format(round(v,0),  big.mark=" ", decimal.mark=","), " €") }

    # connaissance (large -> 3 valeurs)
    read_connaissance_vec <- function(d) {
      if (is.null(d) || !is.data.frame(d) || nrow(d) < 1) return(list(values=NA, labels="Aucune donnée"))
      nm <- names(d)
      pick <- function(patterns) {
        for (p in patterns) {
          idx <- grep(p, nm, ignore.case = TRUE)
          if (length(idx)) return(nm[idx[1]])
        }
        NA_character_
      }
      nc <- pick(c("^conn", "connus?"))
      ne <- pick(c("^estim", "estim[ée]s?"))
      np <- pick(c("pas.*don", "sans.*don"))
      vals <- c(num1(d[1, nc, drop=TRUE]), num1(d[1, ne, drop=TRUE]), num1(d[1, np, drop=TRUE]))
      labs <- c("Connus","Estimés","Pas de données")
      ok <- is.finite(vals) & vals >= 0
      list(values = vals[ok], labels = labs[ok])
    }

    # matériaux (large -> long)
    read_mat_large <- function(d) {
      if (is.null(d) || !is.data.frame(d) || nrow(d) < 1) return(list(values=0, labels="Aucune donnée"))
      nm <- setdiff(names(d), c("texte","Total","total","TOTAL"))
      vals <- suppressWarnings(as.numeric(d[1, nm, drop=TRUE]))
      ok <- is.finite(vals) & vals >= 0
      list(values = vals[ok], labels = nm[ok])
    }

    # état de santé (large -> classes)
    read_etat_vec <- function(d) {
      if (is.null(d) || !is.data.frame(d) || nrow(d) < 1) return(list(values=NA, labels="Aucune donnée"))
      nm <- names(d)
      pick <- function(...){
        pats <- c(...)
        for (p in pats) {
          idx <- grep(p, nm, ignore.case = TRUE)
          if (length(idx)) return(nm[idx[1]])
        }
        NA_character_
      }
      cols <- c(
        bon   = pick("^.?[eé]tat.?\\s*bon$"),
        corr  = pick("^.?[eé]tat.?\\s*cor"),
        degr  = pick("^.?[eé]tat.?\\s*d[ée]gr"),
        mauv  = pick("^.?[eé]tat.?\\s*mau"),
        itv   = pick("itv.*incompl"),
        pdd   = pick("pas.*don")
      )
      labs_order <- c("État bon","État correct","État dégradé","État mauvais","ITV incomplètes","Pas de données")
      vals <- c(num1(d[1, cols["bon"],  drop=TRUE]),
                num1(d[1, cols["corr"], drop=TRUE]),
                num1(d[1, cols["degr"], drop=TRUE]),
                num1(d[1, cols["mauv"], drop=TRUE]),
                num1(d[1, cols["itv"],  drop=TRUE]),
                num1(d[1, cols["pdd"],  drop=TRUE]))
      ok <- is.finite(vals) & vals >= 0
      list(values = vals[ok], labels = labs_order[ok])
    }

    # ---------- Crumb ----------
    output$crumb <- renderUI({
      tagList(
        span(class="crumb-main", paste0("Commune : ", commune())),
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
      tibble::tibble(
        `Libellé` = c("Total EP", "Réseaux visitables (>=1,5m)", "Réseaux non visitables"),
        `Valeur`  = c(fmt_km(.eu_get(s,"LEP")$longueur_sum),
                      fmt_km(.eu_get(s,"LRV")$longueur_visitable),
                      fmt_km(.eu_get(s,"LRV")$longueur_non_visitable))
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=2)

    output$plt_connaissance_mat_bar <- renderPlotly({
      cc <- read_connaissance_vec(.eu_get(sec(),"CON_MAT"))
      .eu_single_hstack(cc$values, cc$labels, "Matériaux — connaissance")
    })
    output$plt_connaissance_age_bar <- renderPlotly({
      cc <- read_connaissance_vec(.eu_get(sec(),"CON_AGE"))
      .eu_single_hstack(cc$values, cc$labels, "Année de pose — connaissance")
    })

    output$plt_tranches_age_total <- renderPlotly({
      d <- .eu_get(sec(),"AR"); req(d, nrow(d) >= 1)
      labs <- setdiff(names(d), c("texte","Total","total"))
      # Pour Commune_EP$AR, c’est “texte + 4 tranches + Total” -> on ignore Total
      vals <- suppressWarnings(as.numeric(d[1, labs, drop=TRUE]))
      .eu_donut(vals, labs, "Tranches d'âge (total)")
    })

    output$plt_precision <- renderPlotly({
      d <- .eu_get(sec(),"LIN_CLASS")
      .eu_donut(d[, c("longueur_sum_A","longueur_sum_B","longueur_sum_C")],
                c("Classe A (< 50cm)","Classe B (< 150cm)","Classe C (> 150cm)"),
                "Précision planimétrique")
    })

    output$plt_materiaux_bar <- renderPlotly({
      mm <- read_mat_large(.eu_get(sec(),"LRM"))
      .eu_bar_materials(mm$values, mm$labels, "Familles de matériaux (total)")
    })

    output$cards_valeurs <- renderUI({
      s <- sec(); req(s)
      vpr <- .eu_get(s,"VPR")$Valeur %||% NA
      npp <- .eu_get(s,"NPPTR")$nbre_parcelle %||% NA
      lrs <- .eu_get(s,"LRSPVP")$lineaire %||% NA
      pct <- .eu_get(s,"LRSPVP")$pourcentage %||% NA
      tagList(
        .eu_info_card("Valeur à neuf réseaux", paste0(fmt_eur(vpr), "\n")),
        .eu_info_card("Parcelles privées traversées",
                      if (is.finite(num1(npp))) format(num1(npp), big.mark=" ", decimal.mark=",") else "—"),
        .eu_info_card("Réseaux sur voies privées",
                      paste0(fmt_km(lrs), " (",
                             if (is.finite(num1(pct))) paste0(round(num1(pct),1), "%") else "—", ")"))
      )
    })

    # ---------- Inventaire > Branchements ----------
    output$plt_br_ce <- renderPlotly({
      d <- .eu_get(sec(),"NBCE")
      .eu_donut(c(d$branch_estimes %||% NA, d$branch_connus %||% NA),
                c("Estimés","Connus"), "Branchements", unit_km = FALSE)
    })
    output$tbl_br <- renderTable({
      s <- sec(); req(s)
      tibble::tibble(
        Indicateur = c("Nombre total", "Valeur à neuf (branchements)"),
        Valeur     = c(format(num1(.eu_get(s,"NBCE")$branch_tot), big.mark=" ", decimal.mark=","),
                       fmt_eur(.eu_get(s,"VP_BR")$VP_branch))
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=0)

    # ---------- Inventaire > Ouvrages ----------
    output$tbl_ouvrages <- renderTable({
      s <- sec(); req(s)
      nap <- .eu_get(s,"NAP")$NBR_AP  %||% NA
      npi <- .eu_get(s,"NPI")$NBR_PI  %||% NA
      nno <- .eu_get(s,"NNO")$NBR_No  %||% NA
      ntr <- .eu_get(s,"NT") $nbr_trmie %||% NA

      vpap <- .eu_get(s,"VP_AP")$VAL_AP   %||% NA
      vppi <- .eu_get(s,"VP_PI")$VAL_PI   %||% NA
      vpog <- .eu_get(s,"VP_OGEP")$VP_OGEP %||% NA
      vptr <- .eu_get(s,"VP_TREM")$VP_TREM %||% NA

      tibble::tibble(
        Ouvrage = c("Avaloirs (AP)", "Puisards (PI)", "Ouvrages de gestion (OGEP)", "Trémies"),
        Nombre  = c(num1(nap), num1(npi), num1(nno), num1(ntr)),
        `Valeur à neuf (€)` = c(fmt_eur(vpap), fmt_eur(vppi), fmt_eur(vpog), fmt_eur(vptr)),
        `Récolements` = c("—","—","—","—"),
        `Foncier à acquérir` = c("—","—","—","—")
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=0)

    output$card_reseau_secs <- renderUI({
      d <- .eu_get(sec(),"LRSFA")
      .eu_info_card(
        "RÉSEAU ÉLECTRIQUE (secs)",
        paste0(" - Total : ",        fmt_m(d$total[1]                 %||% NA), "\n"),
        paste0(" - Haute tension : ",fmt_m(d$longueur_rsec_fort[1]    %||% NA), "\n"),
        paste0(" - Basse tension : ",fmt_m(d$longueur_rsec_faible[1]  %||% NA))
      )
    })

    output$card_precision_secs <- renderUI({
      d <- .eu_get(sec(),"LIN_SEC_CLASS")
      a <- d$longueur_sum_A[1] %||% NA
      b <- d$longueur_sum_B[1] %||% NA
      c <- d$longueur_sum_C[1] %||% NA
      tot <- sum(c(num1(a),num1(b),num1(c)), na.rm = TRUE)
      pct <- function(x) if (is.finite(num1(x)) && tot>0) paste0(" (", round(num1(x)/tot*100,1), "%)") else ""
      .eu_info_card(
        "Précision planimétrique (réseaux secs)",
        paste0("Classe A (< 50cm) : ", fmt_km(a), pct(a), "\n"),
        paste0("Classe B (< 150cm) : ", fmt_km(b), pct(b), "\n"),
        paste0("Classe C (> 150cm) : ", fmt_km(c), pct(c))
      )
    })

    # ---------- État de santé ----------
    output$plt_etat_nv <- renderPlotly({
      ee <- read_etat_vec(.eu_get(sec(),"ES_NV"))
      .eu_donut(ee$values, ee$labels, "Collecteurs non visitables — état")
    })
    output$plt_etat_v <- renderPlotly({
      ee <- read_etat_vec(.eu_get(sec(),"ES_V"))
      .eu_donut(ee$values, ee$labels, "Collecteurs visitables — état")
    })
    output$card_itv_diag <- renderUI({
      s <- sec(); req(s)
      litv <- .eu_get(s,"LITV")
      diag <- .eu_get(s,"LDIAG_PREDIAG_DIAG")
      pre  <- .eu_get(s,"LDIAG_PREDIAG_PREDIAG")
      .eu_info_card(
        "COLLECTEURS : linéaire inspecté",
        paste0("Réseaux non visitables — ITV (km) : ", fmt_km(litv$Total), "\n"),
        paste0("Réseaux visitables — ITV dans l'année : ", fmt_km(litv$Realise_dans_lannee), "\n"),
        if (!is.null(diag)) paste0("Diagnostics (km) : ", fmt_km(diag$Total), "\n") else "",
        if (!is.null(pre))  paste0("Prédiagnostics (km) : ", fmt_km(pre$Total)) else ""
      )
    })
    output$tbl_inspections <- renderTable({
      d <- .eu_get(sec(),"NTR_RECOLES")
      if (is.null(d)) return(tibble::tibble(`Information`="—"))
      tibble::tibble(
        `Nombre recolements` = num1(d$nbre_rec),
        `Total`              = num1(d$nbre_tot),
        `Pourcentage`        = num1(d$Pourcentage)
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=1)

    # ---------- Travaux ----------
    output$card_travaux <- renderUI({
      lrit <- .eu_get(sec(),"LRIT")$long_idee %||% NA
      .eu_info_card("Programme de travaux (EP)",
                    paste0("Idées de travaux — Réseaux : ", fmt_km(lrit)))
    })
  })
}
