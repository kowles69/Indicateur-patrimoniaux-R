# R/Mod_CAPT_EU.R — Périmètre de captage (EU_U)
mod_CAPT_EU_ui <- mod_BV_EP_ui  # même overlay

mod_CAPT_EU_server <- function(id, store_list, perimetre) {
  moduleServer(id, function(input, output, session) {
    # On réutilise exactement le code de CAPT_EP_server en changeant juste la section source -> "EU_U"
    # Pour éviter le copier/coller massif, on appelle CAPT_EP_server-style helpers avec type_key = "EU_U".
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
    .get_capt_store_section <- function(store, type_key){
      keys <- c(
        paste0("Captage_", type_key),
        paste0("Périmètre de captage_", type_key),
        paste0("Perimetre de captage_", type_key),
        paste0("Perimetre captage_", type_key),
        paste0("CAPT_", type_key)
      )
      nm <- names(store %||% list())
      for (k in keys){
        if (!is.null(nm) && k %in% nm) return(store[[k]])
        sec <- try(get_section(store, k), silent = TRUE)
        if (!inherits(sec,"try-error") && !is.null(sec)) return(sec)
      }
      NULL
    }
    .capt_get <- function(sec, capt_name, ...){
      keys <- c(...); if (is.null(sec) || !is.list(sec)) return(NULL)
      obj <- NULL
      for (k in keys) if (!is.null(sec[[k]])) { obj <- sec[[k]]; break }
      if (is.null(obj)) {
        nm <- names(sec);
        for (k in keys){ ix <- grep(k, nm, ignore.case = TRUE, perl = TRUE); if (length(ix)) { obj <- sec[[ nm[ix[1]] ]]; break } }
      }
      d <- .to_df(obj); if (is.null(d) || !nrow(d)) return(NULL)
      col_nom <- .pick_name(d, c("^nomcaptage$","^nom_captage$","^Nom$","^texte$"))
      if (!is.na(col_nom)) {
        clean <- function(s) tolower(trimws(enc2utf8(as.character(s))))
        d <- d[ clean(d[[col_nom]]) == clean(capt_name), , drop = FALSE]
      }
      d
    }

    sec <- reactive({
      req(store_list, perimetre)
      .get_capt_store_section(if (is.function(store_list)) store_list() else store_list, "EU_U")
    })

    # On réutilise exactement les sorties du module CAPT_EP, avec les clés EU :
    output$crumb <- renderUI({
      tagList(
        span(class="crumb-main", paste0("Périmètre de captage : ", perimetre())),
        span(class="crumb-sep", icon("angle-right")),
        span(class="crumb-sub",
             switch(input$main %||% "inv",
                    inv="Inventaire", sante="État de santé",
                    travaux="Programme de travaux", carto="Cartographie"))
      )
    })

    # Inventaire > Collecteurs
    output$tbl_lineaire <- renderTable({
      s <- sec(); req(s)
      lru  <- .capt_get(s, perimetre(), "LRU")
      lreu <- .capt_get(s, perimetre(), "LREU")
      lep  <- .capt_get(s, perimetre(), "LEP")
      lrv  <- .capt_get(s, perimetre(), "LRV")

      num1 <- function(x) suppressWarnings(as.numeric(x))[1]
      pick <- function(d, cand){
        if (is.null(d)) return(NA_real_)
        for (nm in cand) if (!is.null(d[[nm]])) return(num1(d[[nm]][1]))
        NA_real_
      }

      val_lru <- pick(lru,  c("longueur_sum","Total","total","longueur"))
      val_lre <- pick(lreu, c("longueur_sum","Total","total","longueur"))
      val_lep <- pick(lep,  c("longueur_sum","Total","total","longueur"))

      val_vis <- pick(lrv,  c("longueur_visitable","visitable","Longueur_visitable"))
      val_non <- pick(lrv,  c("longueur_non_visitable","non_visitable","non.visitable","Longueur_non_visitable"))

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
      nm <- setdiff(names(d), c("texte","Total","total","TOTAL","nomcaptage","nom_captage","Nom"))
      vals <- suppressWarnings(as.numeric(d[1, nm, drop=TRUE]))
      ok <- is.finite(vals) & vals>=0; list(values=vals[ok], labels=nm[ok])
    }

    output$plt_connaissance_mat_bar <- renderPlotly({
      cc <- read_connaissance_vec(.capt_get(sec(), perimetre(),"ConMat","CON_MAT"))
      .eu_single_hstack(cc$values, cc$labels, "Matériaux — connaissance")
    })
    output$plt_connaissance_age_bar <- renderPlotly({
      cc <- read_connaissance_vec(.capt_get(sec(), perimetre(),"ConAge","CON_AGE"))
      .eu_single_hstack(cc$values, cc$labels, "Année de pose — connaissance")
    })
    output$plt_tranches_age_total <- renderPlotly({
      d <- .capt_get(sec(), perimetre(),"AR"); req(d, nrow(d)>=1)
      labs <- setdiff(names(d), c("texte","Total","total","nomcaptage","nom_captage","Nom"))
      vals <- suppressWarnings(as.numeric(d[1, labs, drop=TRUE]))
      .eu_donut(vals, labs, "Tranches d'âge (total)")
    })
    output$plt_precision <- renderPlotly({
      d <- .capt_get(sec(), perimetre(),"LinClass","LIN_CLASS")
      if (is.null(d)) return(.eu_donut(c(1), c("Aucune donnée"), "Précision planimétrique"))
      .eu_donut(d[, c("longueur_sum_A","longueur_sum_B","longueur_sum_C")],
                c("Classe A (< 50cm)","Classe B (< 150cm)","Classe C (> 150cm)"),
                "Précision planimétrique")
    })
    output$plt_materiaux_bar <- renderPlotly({
      mm <- read_mat_large(.capt_get(sec(), perimetre(),"LRM"))
      .eu_bar_materials(mm$values, mm$labels, "Familles de matériaux (total)")
    })
    output$cards_valeurs <- renderUI({
      s <- sec(); req(s)
      vpr <- (.capt_get(s, perimetre(),"VPR")$Valeur) %||% NA
      npp <- (.capt_get(s, perimetre(),"NPPTR")$nbre_parcelle) %||% NA
      lrs <- (.capt_get(s, perimetre(),"LRSPVP")$lineaire) %||% NA
      pct <- (.capt_get(s, perimetre(),"LRSPVP")$pourcentage) %||% NA
      tagList(
        .eu_info_card("Valeur à neuf réseaux", paste0(fmt_eur(vpr), "\n")),
        .eu_info_card("Parcelles privées traversées",
                      if (is.finite(num1(npp))) format(num1(npp), big.mark=" ", decimal.mark=",") else "—"),
        .eu_info_card("Réseaux sur voies privées",
                      paste0(fmt_km(lrs), " (", if (is.finite(num1(pct))) paste0(round(num1(pct),1), "%") else "—", ")"))
      )
    })

    output$plt_br_ce <- renderPlotly({
      d <- .capt_get(sec(), perimetre(),"NBCE")
      .eu_donut(c(d$branch_estimes %||% NA, d$branch_connus %||% NA),
                c("Estimés","Connus"), "Branchements", unit_km = FALSE)
    })
    output$tbl_br <- renderTable({
      s <- sec(); req(s)
      tibble::tibble(
        Indicateur = c("Nombre total", "Valeur à neuf (branchements)"),
        Valeur     = c(format(num1(.capt_get(s,perimetre(),"NBCE")$branch_tot), big.mark=" ", decimal.mark=","),
                       fmt_eur(.capt_get(s,perimetre(),"VPBr","VP_BR")$VP_branch %||% NA))
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=0)

    output$tbl_ouvrages <- renderTable({
      s <- sec(); req(s)
      nsteu <- .capt_get(s,perimetre(),"NISTEU")$nombre_STEU %||% NA
      nstr  <- .capt_get(s,perimetre(),"NISTR") $nombre_STR  %||% NA
      nfpr  <- .capt_get(s,perimetre(),"NFPR")  $NBR_FPR     %||% NA
      ndo   <- .capt_get(s,perimetre(),"NDO")   $nombre      %||% NA
      ndes  <- .capt_get(s,perimetre(),"NDes")  $nombre      %||% NA
      vp_st <- (.capt_get(s,perimetre(),"VPSTEU")$VP_STEU) %||% NA
      vp_sr <- (.capt_get(s,perimetre(),"VPSTR") $VP_STR)  %||% NA
      vp_fp <- (.capt_get(s,perimetre(),"VPFPR") $VP_FPR)  %||% NA
      tibble::tibble(
        Ouvrage = c("STEU","STR","FPR","Déversoirs d'orage","Dessableurs"),
        Nombre  = c(num1(nsteu), num1(nstr), num1(nfpr), num1(ndo), num1(ndes)),
        `Valeur à neuf (€)` = c(fmt_eur(vp_st), fmt_eur(vp_sr), fmt_eur(vp_fp), "—", "—"),
        `Récolements` = c("—","—","—","—","—"),
        `Foncier à acquérir` = c("—","—","—","—","—")
      )
    }, striped=TRUE, hover=TRUE, spacing="xs", digits=0)

    output$card_reseau_secs <- renderUI({
      d <- .capt_get(sec(), perimetre(),"LRSFa","LRSFA")
      total <- d$total[1]                 %||% NA
      ht    <- d$longueur_rsec_fort[1]    %||% NA
      bt    <- d$longueur_rsec_faible[1]  %||% NA
      .eu_info_card("RÉSEAU ÉLECTRIQUE (secs)",
                    paste0(" - Total : ", fmt_m(total), "\n"),
                    paste0(" - Haute tension : ", fmt_m(ht), "\n"),
                    paste0(" - Basse tension : ", fmt_m(bt)))
    })
    output$card_precision_secs <- renderUI({
      d <- .capt_get(sec(), perimetre(),"LIN_SEC_CLASS")
      a <- d$longueur_sum_A[1] %||% NA; b <- d$longueur_sum_B[1] %||% NA; c <- d$longueur_sum_C[1] %||% NA
      tot <- sum(c(num1(a),num1(b),num1(c)), na.rm = TRUE)
      pct <- function(x) if (is.finite(num1(x)) && tot>0) paste0(" (", round(num1(x)/tot*100,1), "%)") else ""
      .eu_info_card("Précision planimétrique (réseaux secs)",
                    paste0("Classe A (< 50cm) : ", fmt_km(a), pct(a), "\n"),
                    paste0("Classe B (< 150cm) : ", fmt_km(b), pct(b), "\n"),
                    paste0("Classe C (> 150cm) : ", fmt_km(c), pct(c)))
    })

    output$plt_etat_nv <- renderPlotly({
      d <- .capt_get(sec(), perimetre(),"ES_NV")
      if (is.null(d)) return(.eu_donut(1,"Aucune donnée","Collecteurs non visitables — état"))
      nm <- names(d); num <- function(x) suppressWarnings(as.numeric(x))[1]
      pick <- function(...){ pats<-c(...); for (p in pats){ ix<-grep(p,nm,ignore.case=TRUE,perl=TRUE); if(length(ix)) return(nm[ix[1]])}; NA_character_}
      vals <- c(num(d[1,pick("^.?[eé]tat.?\\s*bon$")]), num(d[1,pick("^.?[eé]tat.?\\s*cor")]),
                num(d[1,pick("^.?[eé]tat.?\\s*d[ée]gr")]), num(d[1,pick("^.?[eé]tat.?\\s*mau")]),
                num(d[1,pick("itv.*incompl")]), num(d[1,pick("pas.*don")]))
      labs <- c("État bon","État correct","État dégradé","État mauvais","ITV incomplètes","Pas de données")
      ok <- is.finite(vals) & vals>=0; .eu_donut(vals[ok], labs[ok], "Collecteurs non visitables — état")
    })
    output$plt_etat_v <- renderPlotly({
      d <- .capt_get(sec(), perimetre(),"ES_V")
      if (is.null(d)) return(.eu_donut(1,"Aucune donnée","Collecteurs visitables — état"))
      nm <- names(d); num <- function(x) suppressWarnings(as.numeric(x))[1]
      pick <- function(...){ pats<-c(...); for (p in pats){ ix<-grep(p,nm,ignore.case=TRUE,perl=TRUE); if(length(ix)) return(nm[ix[1]])}; NA_character_}
      vals <- c(num(d[1,pick("^.?[eé]tat.?\\s*bon$")]), num(d[1,pick("^.?[eé]tat.?\\s*cor")]),
                num(d[1,pick("^.?[eé]tat.?\\s*d[ée]gr")]), num(d[1,pick("^.?[eé]tat.?\\s*mau")]),
                num(d[1,pick("itv.*incompl")]), num(d[1,pick("pas.*don")]))
      labs <- c("État bon","État correct","État dégradé","État mauvais","ITV incomplètes","Pas de données")
      ok <- is.finite(vals) & vals>=0; .eu_donut(vals[ok], labs[ok], "Collecteurs visitables — état")
    })

    output$tbl_inspections <- renderTable({ tibble::tibble(Information="—") })
    output$card_travaux <- renderUI({
      lrit <- .capt_get(sec(), perimetre(),"LRIT")$long_idee %||% NA
      .eu_info_card("Programme de travaux (EU)",
                    paste0("Idées de travaux — Réseaux : ", fmt_km(lrit)))
    })
  })
}
