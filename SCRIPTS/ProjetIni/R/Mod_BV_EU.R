# R/Mod_BV_EU.R — Bassin versant (EU_U)

mod_BV_EU_ui <- mod_BV_EP_ui  # même UI

mod_BV_EU_server <- function(id, store_list, bassin) {
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
      if ("python.builtin.object" %in% class(x)) x <- tryCatch(reticulate::py_to_r(x), error=function(e) NULL)
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
      .get_bv_store_section(if (is.function(store_list)) store_list() else store_list, "EU_U")
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

    # Inventaire > Collecteurs
    output$tbl_lineaire <- renderTable({
      s <- sec(); b <- bassin(); req(s, b)

      lru  <- .bv_get(s, b, "LRU")
      lreu <- .bv_get(s, b, "LREU")
      lep  <- .bv_get(s, b, "LEP")
      lrv  <- .bv_get(s, b, "LRV")

      pick_col <- function(d, cand){
        if (is.null(d) || !nrow(d)) return(NA_real_)
        for (nm in cand) if (nm %in% names(d)) return(num1(d[1, nm, drop = TRUE]))
        NA_real_
      }

      val_lru <- pick_col(lru,  c("longueur_sum","Total","total","longueur"))
      val_lre <- pick_col(lreu, c("longueur_sum","Total","total","longueur"))
      val_lep <- pick_col(lep,  c("longueur_sum","Total","total","longueur"))
      val_vis <- pick_col(lrv,  c("longueur_visitable","visitable","Longueur_visitable"))
      val_non <- pick_col(lrv,  c("longueur_non_visitable","non_visitable","non.visitable","Longueur_non_visitable"))

      tibble::tibble(
        `Libellé` = c(
          "Réseaux unitaires (U)",
          "Réseaux eaux usées (EU)",
          "Total EU/U",
          "Réseaux visitables (>=1,5m)",
          "Réseaux non visitables"
        ),
        `Valeur` = c(
          fmt_km(val_lru),
          fmt_km(val_lre),
          fmt_km(val_lep),
          fmt_km(val_vis),
          fmt_km(val_non)
        )
      )
    }, striped = TRUE, hover = TRUE, spacing = "xs", digits = 2)




    read_connaissance_vec <- function(d){
      if (is.null(d) || !nrow(d)) return(list(values=NA, labels="Aucune donnée"))
      nm <- names(d); pick <- function(pats){ for (p in pats){ ix <- grep(p, nm, ignore.case=TRUE); if (length(ix)) return(nm[ix[1]]) }; NA_character_ }
      vals <- c(num1(d[1, pick(c("^%?\\s*conn","connus?")), drop=TRUE]),
                num1(d[1, pick(c("^%?\\s*estim","estim[ée]s?")), drop=TRUE]),
                num1(d[1, pick(c("pas.*don","sans.*don")), drop=TRUE]))
      labs <- c("Connus","Estimés","Pas de données"); ok <- is.finite(vals) & vals>=0
      list(values=vals[ok], labels=labs[ok])
    }
    read_mat_large <- function(d){
      if (is.null(d) || !nrow(d)) return(list(values=0, labels="Aucune donnée"))
      nm <- setdiff(names(d), c("texte","Total","total","TOTAL","Bassin","Nom"))
      vals <- suppressWarnings(as.numeric(d[1, nm, drop=TRUE]))
      ok <- is.finite(vals) & vals>=0; list(values=vals[ok], labels=nm[ok])
    }

    output$plt_connaissance_mat_bar <- renderPlotly({
      cc <- read_connaissance_vec(.bv_get(sec(), bassin(), "ConMat","CON_MAT"))
      .eu_single_hstack(cc$values, cc$labels, "Matériaux — connaissance")
    })
    output$plt_connaissance_age_bar <- renderPlotly({
      cc <- read_connaissance_vec(.bv_get(sec(), bassin(), "ConAge","CON_AGE"))
      .eu_single_hstack(cc$values, cc$labels, "Année de pose — connaissance")
    })
    output$plt_tranches_age_total <- renderPlotly({
      d <- .bv_get(sec(), bassin(),"AR"); req(d, nrow(d)>=1)
      labs <- setdiff(names(d), c("texte","Total","total","Bassin","Nom"))
      vals <- suppressWarnings(as.numeric(d[1, labs, drop=TRUE]))
      .eu_donut(vals, labs, "Tranches d'âge (total)")
    })
    output$plt_precision <- renderPlotly({
      d <- .bv_get(sec(), bassin(),"LinClass","LIN_CLASS")
      if (is.null(d)) return(.eu_donut(c(1), c("Aucune donnée"), "Précision planimétrique"))
      .eu_donut(d[, c("longueur_sum_A","longueur_sum_B","longueur_sum_C")],
                c("Classe A (< 50cm)","Classe B (< 150cm)","Classe C (> 150cm)"),
                "Précision planimétrique")
    })
    output$plt_materiaux_bar <- renderPlotly({
      mm <- read_mat_large(.bv_get(sec(), bassin(),"LRM"))
      .eu_bar_materials(mm$values, mm$labels, "Familles de matériaux (total)")
    })
    output$cards_valeurs <- renderUI({
      s <- sec(); req(s)
      vpr <- (.bv_get(s,bassin(),"VPR")$Valeur) %||% NA
      npp <- (.bv_get(s,bassin(),"NPPTR")$nbre_parcelle) %||% NA
      lrs <- (.bv_get(s,bassin(),"LRSPVP")$lineaire) %||% NA
      pct <- (.bv_get(s,bassin(),"LRSPVP")$pourcentage) %||% NA
      tagList(
        .eu_info_card("Valeur à neuf réseaux", paste0(fmt_eur(vpr), "\n")),
        .eu_info_card("Parcelles privées traversées",
                      if (is.finite(num1(npp))) format(num1(npp), big.mark=" ", decimal.mark=",") else "—"),
        .eu_info_card("Réseaux sur voies privées",
                      paste0(fmt_km(lrs), " (", if (is.finite(num1(pct))) paste0(round(num1(pct),1), "%") else "—", ")"))
      )
    })

    # Branchements
    output$plt_br_ce <- renderPlotly({
      d <- .bv_get(sec(), bassin(),"NBCE")
      .eu_donut(c(d$branch_estimes %||% NA, d$branch_connus %||% NA),
                c("Estimés","Connus"), "Branchements", unit_km = FALSE)
    })
    output$tbl_br <- renderTable({
      s <- sec(); req(s)
      tibble::tibble(
        Indicateur = c("Nombre total", "Valeur à neuf (branchements)"),
        Valeur     = c(format(num1(.bv_get(s,bassin(),"NBCE")$branch_tot), big.mark=" ", decimal.mark=","),
                       fmt_eur(.bv_get(s,bassin(),"VPBr","VP_BR")$VP_branch %||% NA))
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=0)

    # Ouvrages + Secs
    output$tbl_ouvrages <- renderTable({
      s <- sec(); req(s)
      nsteu <- .bv_get(s,bassin(),"NSTEU")$nombre_STEU %||% NA
      nstr  <- .bv_get(s,bassin(),"NSTR") $nombre_STR  %||% NA
      nfpr  <- .bv_get(s,bassin(),"NFPR") $NBR_FPR     %||% NA
      ndo   <- .bv_get(s,bassin(),"NDO")  $nombre      %||% NA
      ndes  <- .bv_get(s,bassin(),"NDes") $nombre      %||% NA
      vp_st <- (.bv_get(s,bassin(),"VPSTEU")$VP_STEU) %||% NA
      vp_sr <- (.bv_get(s,bassin(),"VPSTR") $VP_STR)  %||% NA
      vp_fp <- (.bv_get(s,bassin(),"VPFPR") $VP_FPR)  %||% NA
      tibble::tibble(
        Ouvrage = c("STEU","STR","FPR","Déversoirs d'orage","Dessableurs"),
        Nombre  = c(num1(nsteu), num1(nstr), num1(nfpr), num1(ndo), num1(ndes)),
        `Valeur à neuf (€)` = c(fmt_eur(vp_st), fmt_eur(vp_sr), fmt_eur(vp_fp), "—", "—"),
        `Récolements` = c("—","—","—","—","—"),
        `Foncier à acquérir` = c("—","—","—","—","—")
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=0)

    output$card_reseau_secs <- renderUI({
      d <- .bv_get(sec(), bassin(),"LRSFa","LRSFA")
      total <- d$total[1]                 %||% NA
      ht    <- d$longueur_rsec_fort[1]    %||% NA
      bt    <- d$longueur_rsec_faible[1]  %||% NA
      .eu_info_card("RÉSEAU ÉLECTRIQUE (secs)",
                    paste0(" - Total : ", fmt_m(total), "\n"),
                    paste0(" - Haute tension : ", fmt_m(ht), "\n"),
                    paste0(" - Basse tension : ", fmt_m(bt)))
    })
    output$card_precision_secs <- renderUI({
      d <- .bv_get(sec(), bassin(),"LIN_SEC_CLASS")
      a <- d$longueur_sum_A[1] %||% NA; b <- d$longueur_sum_B[1] %||% NA; c <- d$longueur_sum_C[1] %||% NA
      tot <- sum(c(num1(a),num1(b),num1(c)), na.rm = TRUE)
      pct <- function(x) if (is.finite(num1(x)) && tot>0) paste0(" (", round(num1(x)/tot*100,1), "%)") else ""
      .eu_info_card("Précision planimétrique (réseaux secs)",
                    paste0("Classe A (< 50cm) : ", fmt_km(a), pct(a), "\n"),
                    paste0("Classe B (< 150cm) : ", fmt_km(b), pct(b), "\n"),
                    paste0("Classe C (> 150cm) : ", fmt_km(c), pct(c)))
    })

    # État de santé + Travaux
    output$plt_etat_nv <- renderPlotly({
      d <- .bv_get(sec(), bassin(),"ES_NV"); if (is.null(d)) return(.eu_donut(1,"Aucune donnée","Collecteurs non visitables — état"))
      nm <- names(d); pick <- function(...){ pats<-c(...); for (p in pats){ ix<-grep(p,nm,ignore.case=TRUE,perl=TRUE); if(length(ix)) return(nm[ix[1]])}; NA_character_}
      vals <- c(num1(d[1,pick("^.?[eé]tat.?\\s*bon$")]), num1(d[1,pick("^.?[eé]tat.?\\s*cor")]),
                num1(d[1,pick("^.?[eé]tat.?\\s*d[ée]gr")]), num1(d[1,pick("^.?[eé]tat.?\\s*mau")]),
                num1(d[1,pick("itv.*incompl")]), num1(d[1,pick("pas.*don")]))
      labs <- c("État bon","État correct","État dégradé","État mauvais","ITV incomplètes","Pas de données")
      ok <- is.finite(vals) & vals>=0; .eu_donut(vals[ok], labs[ok], "Collecteurs non visitables — état")
    })
    output$plt_etat_v <- renderPlotly({
      d <- .bv_get(sec(), bassin(),"ES_V"); if (is.null(d)) return(.eu_donut(1,"Aucune donnée","Collecteurs visitables — état"))
      nm <- names(d); pick <- function(...){ pats<-c(...); for (p in pats){ ix<-grep(p,nm,ignore.case=TRUE,perl=TRUE); if(length(ix)) return(nm[ix[1]])}; NA_character_}
      vals <- c(num1(d[1,pick("^.?[eé]tat.?\\s*bon$")]), num1(d[1,pick("^.?[eé]tat.?\\s*cor")]),
                num1(d[1,pick("^.?[eé]tat.?\\s*d[ée]gr")]), num1(d[1,pick("^.?[eé]tat.?\\s*mau")]),
                num1(d[1,pick("itv.*incompl")]), num1(d[1,pick("pas.*don")]))
      labs <- c("État bon","État correct","État dégradé","État mauvais","ITV incomplètes","Pas de données")
      ok <- is.finite(vals) & vals>=0; .eu_donut(vals[ok], labs[ok], "Collecteurs visitables — état")
    })
    output$tbl_inspections <- renderTable({ tibble::tibble(Information="—") })

    output$card_travaux <- renderUI({
      lrit <- .bv_get(sec(), bassin(),"LRIT")$long_idee %||% NA
      .eu_info_card("Programme de travaux (EU)",
                    paste0("Idées de travaux — Réseaux : ", fmt_km(lrit)))
    })

  })
}
