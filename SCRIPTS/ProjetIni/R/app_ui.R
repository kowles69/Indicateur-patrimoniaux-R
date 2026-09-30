#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("thematique"), "Thématique", choices = c("Sélectionner", names(data))),
    selectInput(ns("type_reseau"), "Type de réseau", choices = c("Sélectionner", "Eau pluviale", "Eau usée")),
    selectInput(ns("nom"), "Nom", choices = c("Sélectionner"))
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ProjetIni"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
#' Application User Interface
#'
#' @import shiny
#' @export
app_ui <- function() {
  fluidPage(
    titlePanel("Données et indicateurs des patrimoines "),

    sidebarLayout(
      sidebarPanel(
        mod_dashboard_ui("dashboard")  # Intégration du module UI
      ),

      mainPanel(
        textOutput("test_output")  # Juste pour tester si l'UI charge bien
      )
    )
  )
}
