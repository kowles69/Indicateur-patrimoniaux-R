# app.R — App principale

options(encoding = "UTF-8")

Sys.setenv(RETICULATE_PYTHON = "c:/Users/IZI0593/Desktop/Indicateurs/env_indicateurs/Scripts/python.exe")
library(reticulate); try(use_python(Sys.getenv("RETICULATE_PYTHON"), required = TRUE), silent = TRUE)

library(shiny); library(shinyjs); library(plotly); library(ggplot2);library(tibble)

options(
  shiny.fullstacktrace = TRUE,
  shiny.sanitize.errors = FALSE,
  shiny.error = function() { cat("\n=== TRACEBACK ===\n"); traceback(2); cat("=== /TRACEBACK ===\n") }
)



`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
source("R/utils_ui_helpers.R", local = TRUE, encoding = "UTF-8")
source("R/sa_mapping.R",       local = TRUE, encoding = "UTF-8")
source("R/utils_data.R",    encoding = "UTF-8")
source("R/utils_data_sa.R", encoding = "UTF-8")
source("R/Mod_M_EP.R");  source("R/Mod_M_EU.R")
source("R/Mod_C_EU.R");  source("R/Mod_C_EP.R")
source("R/Mod_SA_EP.R"); source("R/Mod_SA_EU.R")
source("R/Mod_BV_EP.R"); source("R/Mod_BV_EU.R")
source("R/Mod_CAPT_EP.R"); source("R/Mod_CAPT_EU.R")

switch_theme_js <- "
Shiny.addCustomMessageHandler('switchTheme', function(message) {
  document.body.classList.remove('ep-theme', 'eu-theme');
  var els = document.querySelectorAll('.theme_class');
  els.forEach(function(el){ el.classList.remove('ep-theme', 'eu-theme'); });

  var btnEU = document.getElementById('btn_eu');
  var btnEP = document.getElementById('btn_ep');
  if (btnEU && btnEP) {
    btnEU.classList.remove('active-eu');
    btnEP.classList.remove('active-ep');
  }

  if (message === 'ep') {
    document.body.classList.add('ep-theme');
    els.forEach(function(el){ el.classList.add('ep-theme'); });
    if (btnEP) btnEP.classList.add('active-ep');
  }
  if (message === 'eu') {
    document.body.classList.add('eu-theme');
    els.forEach(function(el){ el.classList.add('eu-theme'); });
    if (btnEU) btnEU.classList.add('active-eu');
  }
});
"

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    # 1) Script de thème (inchangé)
    tags$script(HTML(switch_theme_js)),

    # 2) Logger Shiny (dans un <script> séparé)
    tags$script(HTML("
    (function(){
      function log(){ if (window.console && console.log) console.log.apply(console, arguments); }
      $(document).on('shiny:connected',    function(){ log('[shiny] connected'); });
      $(document).on('shiny:disconnected', function(){ log('[shiny] disconnected'); });
      $(document).on('shiny:error',        function(e){ log('[shiny] error:', e.originalEvent && e.originalEvent.error); });
      $(document).on('shiny:message',      function(e){ /* ping */ });
    })();
  ")),

    # 3) Feuille de style
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),

  # Barre
  div(class="navbar-fixed",
      div(class="navbar-left",
          tags$img(src="logo_lyon.png", alt="Logo Metropole de Lyon",
                   onerror="this.style.display='none'", class="navbar-logo"),
          tags$span("Donnees et indicateurs - Metropole de Lyon", class="navbar-title")
      ),
      div(class="navbar-buttons",
          downloadButton("export_pdf", "Exporter PDF", class="btn btn-light"),
          actionButton("go_home", "Accueil", class="btn btn-light")
      )
  ),

  # Sélecteurs haut
  fluidRow(
    column(
      3,
      selectInput(
        "thematique", NULL,
        choices = c(
          "Echelle thematique" = "",
          "Metropole"          = "METRO",
          "Commune"            = "COM",
          "Systeme d'assainissement" = "SA",
          "Bassin versant"     = "BV",
          "Perimetre de captage" = "CAPT"
        ),
        selected = "METRO"
      )
    ),
    column(3, uiOutput("choix_nom", container = div, inline = TRUE)),

    column(6,
           div(id="type_eau_selector",
               actionButton("btn_eu", label = tagList(icon("tint"), "Eaux Usees"), class="btn-eau active-eu"),
               actionButton("btn_ep", label = tagList(icon("cloud-rain"), "Eaux Pluviales"), class="btn-eau")
           )
    )
  ),

  uiOutput("ui_main")
)

server <- function(input, output, session) {

  session$allowReconnect(TRUE)
  session$onSessionEnded(function(){
    message('[session] ended at ', Sys.time())
  })

  # Type d’eau
  type_eau_reactif <- reactiveVal("Eau usée")
  observeEvent(input$btn_ep, { type_eau_reactif("Eau pluviale"); session$sendCustomMessage("switchTheme", "ep") })
  observeEvent(input$btn_eu, { type_eau_reactif("Eau usée");    session$sendCustomMessage("switchTheme", "eu") })

  # Clé EP / EU_U sans passer par tolower() (robuste encodage)
  type_key_reactif <- reactive({
    if (identical(type_eau_reactif(), "Eau pluviale")) "EP" else "EU_U"
  })

  # Helpers
  safe_mod <- function(expr, scope = "") {
    tryCatch(force(expr), error = function(e) {
      showNotification(paste0("Erreur dans ", scope, " : ", conditionMessage(e)), type = "error", duration = NULL)
      message(">>> [", scope, "] ", conditionMessage(e))
    })
  }

  .theme_key <- function(x) {
    x <- as.character(x)
    if (x %in% c("METRO","COM","SA","BV","CAPT","")) return(x)
    s <- tolower(iconv(enc2utf8(x), to = "ASCII//TRANSLIT"))
    if (grepl("metropole", s))                    return("METRO")
    if (grepl("^\\s*commune\\b", s))              return("COM")
    if (grepl("systeme d.?assainissement", s))    return("SA")
    if (grepl("bassin\\s*versant", s))            return("BV")
    if (grepl("perimetre\\s*de\\s*captage", s))   return("CAPT")
    ""
  }

  .clean_vals <- function(v) {
    v <- trimws(enc2utf8(as.character(v))); v <- v[!is.na(v) & nzchar(v)]
    vn <- tolower(gsub("[^[:alnum:]]+", "", v))
    bad <- c("na","nan","null","none","pasdedonnees","pasdedonnées","0","false","true")
    v[!(vn %in% bad)]
  }

  .find_section_quiet <- function(store_list, keys) {
    nm <- names(store_list %||% list())
    for (k in keys) {
      if (!is.null(nm) && k %in% nm) return(store_list[[k]])
      sec <- try(get_section(store_list, k), silent = TRUE)
      if (!inherits(sec, "try-error") && !is.null(sec)) return(sec)
    }
    NULL
  }

  .collect_unique <- function(x, aliases) {
    out <- character()
    walk_obj <- function(obj) {
      if (is.data.frame(obj)) {
        for (al in aliases) if (al %in% names(obj)) out <<- c(out, .clean_vals(obj[[al]]))
      } else if (is.list(obj)) for (el in obj) walk_obj(el)
    }
    walk_obj(x); sort(unique(out))
  }

  .list_bv_names <- function(store_obj, type_key) {
    if (is.null(store_obj) || is.na(type_key)) return(character())
    sec <- .find_section_quiet(store_obj, c(
      paste0("Bassin versant_", type_key),
      paste0("Bassin versant ", type_key),
      paste0("Bassin_versant_", type_key),
      paste0("BV_", type_key)
    ))
    out <- .collect_unique(sec, c("Bassin","bassin","Bassin versant","bassin_versant","Nom","texte"))
    out <- out[nzchar(out)]
    out <- out[!grepl("^(pas\\s*de\\s*don|false|null|na|0)$", out, ignore.case = TRUE)]
    sort(unique(out))
  }

  .list_capt_names <- function(store_obj, type_key) {
    if (is.null(store_obj) || is.na(type_key)) return(character())
    sec <- .find_section_quiet(store_obj, c(
      paste0("Captage_", type_key),
      paste0("Périmètre de captage_", type_key),
      paste0("Perimetre de captage_", type_key),
      paste0("Perimetre captage_", type_key),
      paste0("CAPT_", type_key)
    ))
    out <- .collect_unique(sec, c("nomcaptage","nom_captage","Nom","texte"))
    out <- out[nzchar(out)]
    out <- out[!grepl("^(pas\\s*de\\s*don|false|null|na|0)$", out, ignore.case = TRUE)]
    sort(unique(out))
  }

  # Données (pickle)
  store <- reactiveVal(NULL)
  observe({ store(load_combined_pickle("data/all_results_combined.pickle")) })

  # ================= Sélecteur central =================
  output$choix_nom <- renderUI({
    req(input$thematique, store())
    th <- .theme_key(input$thematique)

    div(id = paste0("chooser-", th),
        if (th == "COM") {
          communes <- list_communes(store)
          if (!length(communes)) communes <- "Pas de donnees"
          selectInput("sel_com", NULL, choices = communes, selected = communes[1], width = "100%")

        } else if (th == "SA") {
          sa <- tryCatch({
            x <- sa_choices(store)
            message("[SA] choices: ", if (!length(x)) "<vide>" else paste(names(x), "=", x, collapse=" | "))
            x
          }, error = function(e) {
            showNotification(paste("Erreur liste SA:", conditionMessage(e)), type = "error", duration = NULL)
            message("!!! [SA choices] ", conditionMessage(e)); character(0)
          })
          if (!length(sa)) sa <- c("Pas de donnees" = "Pas de donnees")
          selectInput("sel_sa", NULL, choices = sa, selected = unname(sa[1]), width = "100%")

        } else if (th == "BV") {
          type_key <- type_key_reactif()
          noms <- .list_bv_names(store(), type_key)
          selectInput("sel_bv", NULL, choices = noms, selected = noms[1], width = "100%")

        } else if (th == "CAPT") {
          type_key <- type_key_reactif()
          noms <- .list_capt_names(store(), type_key)
          selectInput("sel_capt", NULL, choices = noms, selected = noms[1], width = "100%")

        } else {
          NULL
        }
    )
  })
  outputOptions(output, "choix_nom", suspendWhenHidden = FALSE)

  # Sync du sélecteur central quand on change d’onglet
  observeEvent(input$thematique, {
    th <- .theme_key(input$thematique)
    if (th == "COM") {
      communes <- list_communes(store); if (!length(communes)) communes <- "Pas de donnees"
      updateSelectInput(session, "sel_com", choices = communes, selected = communes[1])

    } else if (th == "SA") {
      sa <- sa_choices(store); if (!length(sa)) sa <- c("Pas de donnees" = "Pas de donnees")
      updateSelectInput(session, "sel_sa", choices = sa, selected = unname(sa[1]))

    } else if (th == "BV") {
      type_key <- type_key_reactif()
      noms <- .list_bv_names(store(), type_key)
      updateSelectInput(session, "sel_bv", choices = noms, selected = noms[1])

    } else if (th == "CAPT") {
      type_key <- type_key_reactif()
      noms <- .list_capt_names(store(), type_key)
      updateSelectInput(session, "sel_capt", choices = noms, selected = noms[1])
    }
  }, ignoreInit = TRUE)

  # Recharger listes au toggle EU/EP
  observeEvent(type_eau_reactif(), {
    th <- .theme_key(input$thematique)
    type_key <- type_key_reactif()

    if (th == "BV") {
      noms <- .list_bv_names(store(), type_key)
      updateSelectInput(session, "sel_bv", choices = noms, selected = noms[1])

    } else if (th == "CAPT") {
      noms <- .list_capt_names(store(), type_key)
      updateSelectInput(session, "sel_capt", choices = noms, selected = noms[1])

    } else if (th == "SA") {
      sa <- sa_choices(store)
      updateSelectInput(session, "sel_sa", choices = sa, selected = unname(sa[1]))
    }
  }, ignoreInit = TRUE)

  # ================= UI centrale =================
  output$ui_main <- renderUI({
    req(store())
    th <- .theme_key(input$thematique)
    message("[ui_main] theme=", th, " / type_eau=", type_eau_reactif())

    div(id = paste0("main-", th),
        tryCatch({
          if (th == "METRO") {
            if (type_eau_reactif() == "Eau pluviale") mod_M_EP_ui("mod_m_ep") else mod_M_EU_ui("mod_m_eu")

          } else if (th == "COM") {
            if (type_eau_reactif() == "Eau pluviale") mod_C_EP_ui("mod_c_ep") else mod_C_EU_ui("mod_c_eu")

          } else if (th == "SA") {
            if (type_eau_reactif() == "Eau pluviale") mod_SA_EP_ui("mod_sa_ep") else mod_SA_EU_ui("mod_sa_eu")

          } else if (th == "BV") {
            if (type_eau_reactif() == "Eau pluviale") mod_BV_EP_ui("mod_bv_ep") else mod_BV_EU_ui("mod_bv_eu")

          } else if (th == "CAPT") {
            if (type_eau_reactif() == "Eau pluviale") mod_CAPT_EP_ui("mod_capt_ep") else mod_CAPT_EU_ui("mod_capt_eu")

          } else {
            div(class="onglet-bloc", p("Selectionne une echelle et le type d’eau."))
          }
        }, error = function(e) {
          showNotification(paste("Erreur construction UI (", th, "):", conditionMessage(e)),
                           type = "error", duration = NULL)
          message("!!! [ui_main/", th, "] ", conditionMessage(e))
          div(class="onglet-bloc", p("Une erreur est survenue dans l’UI ", th, "."))
        })
    )
  })
  outputOptions(output, "ui_main", suspendWhenHidden = FALSE)

  # ================= Lancement modules =================
  observeEvent(
    list(type_eau_reactif(), input$thematique, input$sel_com, input$sel_sa, input$sel_bv, input$sel_capt),
    {
      req(store())
      th <- .theme_key(input$thematique)

      bad_sel <- function(x){
        if (is.null(x)) return(TRUE)
        xnorm <- tolower(gsub("[^[:alnum:]]+", "", as.character(x)))
        xnorm %in% c("","pasdedonnees","false","true","na","nan","null","0")
      }

      if (th == "METRO") {
        if (type_eau_reactif() == "Eau pluviale") safe_mod(mod_M_EP_server("mod_m_ep", store_list = store()), "METRO_EP")
        else safe_mod(mod_M_EU_server("mod_m_eu", store_list = store()), "METRO_EU")

      } else if (th == "COM") {
        sel <- input$sel_com; if (bad_sel(sel)) return()
        commune_r <- reactive(sel)
        if (type_eau_reactif() == "Eau pluviale") safe_mod(mod_C_EP_server("mod_c_ep", store_list = store(), commune = commune_r), "COM_EP")
        else safe_mod(mod_C_EU_server("mod_c_eu", store_list = store(), commune = commune_r), "COM_EU")

      } else if (th == "SA") {
        sel <- input$sel_sa; if (bad_sel(sel)) return()
        sa_code_r <- reactive(sel)
        if (type_eau_reactif() == "Eau pluviale") safe_mod(mod_SA_EP_server("mod_sa_ep", store_list = store(), sa_code = sa_code_r), "SA_EP")
        else safe_mod(mod_SA_EU_server("mod_sa_eu", store_list = store(), sa_code = sa_code_r), "SA_EU")

      } else if (th == "BV") {
        sel <- input$sel_bv; if (bad_sel(sel)) return()
        bassin_r <- reactive(sel)
        if (type_eau_reactif() == "Eau pluviale") safe_mod(mod_BV_EP_server("mod_bv_ep", store_list = store(), bassin = bassin_r), "BV_EP")
        else safe_mod(mod_BV_EU_server("mod_bv_eu", store_list = store(), bassin = bassin_r), "BV_EU")

      } else if (th == "CAPT") {
        sel <- input$sel_capt; if (bad_sel(sel)) return()
        perimetre_r <- reactive(sel)
        if (type_eau_reactif() == "Eau pluviale") safe_mod(mod_CAPT_EP_server("mod_capt_ep", store_list = store(), perimetre = perimetre_r), "CAPT_EP")
        else safe_mod(mod_CAPT_EU_server("mod_capt_eu", store_list = store(), perimetre = perimetre_r), "CAPT_EU")
      }
    },
    ignoreInit = FALSE
  )

  observe({ session$sendCustomMessage("switchTheme", if (type_eau_reactif()=="Eau pluviale") "ep" else "eu") })
}

shinyApp(ui = ui, server = server)
