options(encoding = "UTF-8")
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinyjs)
library(rlang)
library(readr)
library(dplyr)

source("R/Mod_M_EP.R")


safe_pie <- function(data, title = "") {
  if (is.null(data) || !is.numeric(data) || any(is.na(data)) || length(data) == 0 || sum(data, na.rm = TRUE) == 0) {
    data <- c("Aucune donnée" = 1)
  }
  df <- data.frame(
    label = names(data),
    value = as.numeric(data)
  )
  df$percentage <- round(df$value / sum(df$value) * 100, 1)
  df$label_text <- paste0(df$label, "\n", df$value, " (", df$percentage, "%)")

  ggplot(df, aes(x = 2, y = value, fill = label)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = ifelse(value > 0, paste0(value, " (", percentage, "%)"), "")),
              position = position_stack(vjust = 0.5), size = 4) +
    xlim(0.5, 2.5) +
    theme_void() +
    labs(title = title, fill = NULL) +
    theme(legend.position = "bottom")
}



ui <- fluidPage(

  useShinyjs(),

  tags$head(
    tags$script(HTML("Shiny.addCustomMessageHandler('switchTheme', function(message) {
  // Enlever les classes des éléments
  document.body.classList.remove('ep-theme', 'eu-theme');
  var themeElements = document.querySelectorAll('.theme_class');
  themeElements.forEach(function(element) {
    element.classList.remove('ep-theme', 'eu-theme');
  });

  // Ajouter la nouvelle classe aux éléments
  if (message === 'ep') {
    document.body.classList.add('ep-theme');
    themeElements.forEach(function(element) {
      element.classList.add('ep-theme');
    });
  } else if (message === 'eu') {
    document.body.classList.add('eu-theme');
    themeElements.forEach(function(element) {
      element.classList.add('eu-theme');
    });
  }
});")),

    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  # Barre fixe
  div(class = "navbar-fixed",
      div(class = "navbar-left",
          tags$img(src = "logo_lyon.png", alt = "Logo Métropole de Lyon",
                   onerror = "this.style.display='none'", class = "navbar-logo"),
          tags$span("Données et indicateurs - Métropole de Lyon", class = "navbar-title")
      ),
      div(class = "navbar-buttons",
          downloadButton("export_pdf", "Exporter PDF", class = "btn btn-light"),
          actionButton("go_home", "Accueil", class = "btn btn-light")
      )
  ),

  # Sélection commune + boutons EP/EU
  fluidRow(
    column(3, selectInput("thematique", NULL,
                          choices = c("Échelle thématique" = "", "Métropole", "Commune", "Système d'assainissement", "Bassin versant", "Captage"),
                          selected = "")
    ),
    column(3, uiOutput("choix_nom")),
    column(6,
           div(id = "type_eau_selector",
               actionButton("btn_eu", label = tagList(icon("tint"), "Eaux Usées"), class = "btn-eau active-eu"),
               actionButton("btn_ep", label = tagList(icon("cloud-rain"), "Eaux Pluviales"), class = "btn-eau")
           )
    )
  ),

  # Bloc thématique dynamique avec fond coloré EP/EU
      tabsetPanel(
        tabPanel(div(icon("map"), "Inventaire des patrimoines"),
                 tabsetPanel(
                   tabPanel(div(icon("sitemap"), "Collecteurs"),
                            div(class = "bandeau", tagList(icon("sitemap"), "Collecteurs")),
                            div(class = "onglet-bloc",
                                fluidRow(
                                  column(4, plotOutput("plot_lineaire")),
                                  column(4, plotOutput("plot_precision")),
                                  column(4, plotOutput("plot_materiaux"))
                                ),
                                fluidRow(
                                  column(4, plotOutput("plot_tranches")),
                                  column(4, verbatimTextOutput("valeur_neuf")),
                                  column(4, verbatimTextOutput("servitudes"))
                                )
                            )
                   ),
                   tabPanel(div(icon("plug"), "Branchements"),
                            div(class = "bandeau", tagList(icon("plug"), "Branchements")),
                            div(class = "onglet-bloc",
                                fluidRow(
                                  column(6, plotOutput("plot_branchements")),
                                  column(6, verbatimTextOutput("valeur_branchements"))
                                )
                            )
                   ),
                   tabPanel(div(icon("cogs"), "Ouvrages"),
                            div(class = "bandeau", tagList(icon("cogs"), "Ouvrages")),
                            div(class = "onglet-bloc",
                                fluidRow(
                                  column(4, plotOutput("plot_ouvrages")),
                                  column(4, verbatimTextOutput("valeur_ouvrages")),
                                  column(4, verbatimTextOutput("recolt_ouvrages"))
                                ),
                                fluidRow(
                                  column(6, plotOutput("age_STEU")),
                                  column(6, plotOutput("age_STR"))
                                ),
                                fluidRow(
                                  column(6, verbatimTextOutput("reseau_elec")),
                                  column(6, verbatimTextOutput("precision_elec"))
                            )
                        )
                   )
                 )
        ),

        tabPanel(div(icon("heartbeat"), "État de santé"),
                 tabsetPanel(
                   tabPanel(div(icon("sitemap"), "Collecteurs"),
                            div(class = "bandeau", tagList(icon("sitemap"), "Collecteurs")),
                            div(class = "onglet-bloc",
                                fluidRow(
                                  column(6, plotOutput("plot_etat_nv")),
                                  column(6, plotOutput("plot_etat_v"))
                                )
                            )
                   ),
                   tabPanel(div(icon("plug"), "Branchements"),
                            div(class = "bandeau", tagList(icon("plug"), "Branchements")),
                            div(class = "onglet-bloc",
                                fluidRow(
                                  column(4, plotOutput("etat_STEU")),
                                  column(4, plotOutput("etat_STR")),
                                  column(4, plotOutput("etat_FPR"))
                                )
                            )
                   ),
                   tabPanel(div(icon("cogs"), "Ouvrages"),
                            div(class = "bandeau", tagList(icon("cogs"), "Ouvrages")),
                            div(class = "onglet-bloc", verbatimTextOutput("inspect_out"))
                   )
                 )
        ),

        tabPanel(div(icon("wrench"), "Programme de travaux"),
                 div(class = "bandeau", tagList(icon("wrench"), "Programme de travaux")),
                 div(class = "onglet-bloc",
                     fluidRow(column(6, verbatimTextOutput("travaux_out")))
                 )
        ),
        tabPanel(div(icon("globe"), "Cartographie"),
                 div(class = "bandeau", tagList(icon("globe"), "Cartographie")),
                 div(class = "onglet-bloc",
                     h3("À venir : module cartographique"),
                 )
        )

      )
  )






server <- function(input, output, session) {

  # Stocke le type de réseau sélectionné (EU/EP)
  type_eau_reactif <- reactiveVal("Eau usée")


  # Gestion du bouton EP
  observeEvent(input$btn_ep, {
    type_eau_reactif("Eau pluviale")
    session$sendCustomMessage("switchTheme", "ep")
    shinyjs::removeClass("btn_eu", class = "active-eu")
    shinyjs::addClass("btn_ep", class = "active-ep")
  })

  # Gestion du bouton EU
  observeEvent(input$btn_eu, {
    type_eau_reactif("Eau usée")
    session$sendCustomMessage("switchTheme", "eu")
    shinyjs::removeClass("btn_ep", class = "active-ep")
    shinyjs::addClass("btn_eu", class = "active-eu")
  })


  output$choix_nom <- renderUI({
    req(input$thematique)

    choix <- switch(input$thematique,
                    "Commune" = c("Choisir un nom" = "", "ALBIGNY-SUR-SAONE"),
                    "Système d'assainissement" = c("Choisir un nom" = "", "STEU_A", "STEU_B"),
                    "Bassin versant" = c("Choisir un nom" = "", "Bassin A", "Bassin B"),
                    "Captage" = c("Choisir un nom" = "", "Captage 1", "Captage 2"),
                    "Métropole" = character(0)
    )

    if (length(choix) == 0) return(NULL)

    selectInput("nom", NULL, choices = choix, selected = "")
  })



  # Récupération des données en fonction du choix
  selected_data <- reactive({
    req(input$thematique, type_eau_reactif())
    get_data_indicateurs(input$thematique, type_eau_reactif())
  })


  output$plot_lineaire <- renderPlot({ safe_pie(selected_data()$lineaire, "Linéaire (km)") })
  output$plot_precision <- renderPlot({ safe_pie(selected_data()$precision, "Précision planimétrique") })
  output$plot_materiaux <- renderPlot({ safe_pie(selected_data()$materiaux, "Matériaux") })
  output$plot_tranches <- renderPlot({ safe_pie(selected_data()$tranches_age, "Tranches d'âge") })
  output$plot_branchements <- renderPlot({ safe_pie(selected_data()$branchements, "Branchements") })
  output$plot_ouvrages <- renderPlot({ safe_pie(selected_data()$ouvrages, "Ouvrages") })
  output$plot_etat_nv <- renderPlot({ safe_pie(selected_data()$etat_sante$non_visibles, "Collecteurs non visitables") })
  output$plot_etat_v <- renderPlot({ safe_pie(selected_data()$etat_sante$visibles, "Collecteurs visitables") })
  output$etat_STEU <- renderPlot({ safe_pie(selected_data()$etat_sante$ouvrages$STEU, "STEU") })
  output$etat_STR <- renderPlot({ safe_pie(selected_data()$etat_sante$ouvrages$STR, "STR") })
  output$etat_FPR <- renderPlot({ safe_pie(selected_data()$etat_sante$ouvrages$FPR, "FPR") })

  output$valeur_neuf <- renderPrint({
    v <- selected_data()$valeur_neuf
    cat("Valeur à neuf réseaux :", v$reseaux)
  })

  output$valeur_branchements <- renderPrint({
    v <- selected_data()$valeur_neuf
    cat("Valeur à neuf branchements :", v$branchements)
  })

  output$servitudes <- renderPrint({
    s <- selected_data()$servitudes
    cat("Parcelles privées traversées :", s$parcelles,
        "\nRéseaux sur voies privées :", s$reseaux_prives_km, "km")
  })

  output$inspect_out <- renderPrint({
    insp <- selected_data()$inspections
    for (type in names(insp)) {
      cat(type, ": Nombre:", insp[[type]]$nombre,
          "| Inspecté:", insp[[type]]$inspecte,
          "| Total:", insp[[type]]$total, "\n")
    }
  })

  output$travaux_out <- renderPrint({
    t <- selected_data()$travaux
    cat("Total réhabilité réseaux:", t$total_rehab,
        "\nBranch. réhabilités:", t$branchements,
        "\nSTEU:", t$STEU,
        "\nSTR:", t$STR,
        "\nFPR:", t$FPR)
  })



  output$reseau_elec <- renderPrint({
    r <- selected_data()$reseau_elec
    cat("RESEAU ELECTRIQUE\n",
        "  - Total :", r$total, "\n",
        "  - Haute Tension :", r$haute, "\n",
        "  - Basse Tension :", r$basse)
  })

  output$precision_elec <- renderPrint({
    p <- selected_data()$precision_elec
    cat("PRECISION PLANIMETRIQUE\n",
        "  - Classe A (< 50cm) :", p$a, "\n",
        "  - Classe B (< 150cm) :", p$b, "\n",
        "  - Classe C (> 150cm) :", p$c)
  })


  output$valeur_ouvrages <- renderPrint({
    v <- selected_data()$valeur_neuf
    cat("Valeur à neuf ouvrages :", v$ouvrages, "M€")
  })

  output$recolt_ouvrages <- renderPrint({
    # On suppose que tu as une clé `recoltements` dans `ouvrages_infos`
    r <- selected_data()$ouvrages_infos
    cat("Récoltement ouvrages :", r$recolt_bassins)
  })

  output$age_STEU <- renderPlot({
    safe_pie(selected_data()$etat_sante$ouvrages$STEU, "STEU (tranches d'âge)")
  })

  output$age_STR <- renderPlot({
    safe_pie(selected_data()$etat_sante$ouvrages$STR, "STR (tranches d'âge)")
  })


  # Appliquer le thème initial
  observe({
    # Cette fonction s'exécute une fois au démarrage
    session$sendCustomMessage("switchTheme", "eu")  # Thème par défaut
  })


}


shinyApp(ui, server)
