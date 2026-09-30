#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

mod_dashboard_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$thematique, {
      req(input$thematique)

      # Mise à jour des types de réseau
      updateSelectInput(session, "type_reseau", choices = c("Sélectionner", "Eau pluviale", "Eau usée"))

      # Si la thématique est Métropole, on vide la liste des noms
      if (input$thematique == "Métropole") {
        updateSelectInput(session, "nom", choices = c("Sélectionner"))
      } else {
        updateSelectInput(session, "nom", choices = c("Sélectionner"))  # Réinitialisation
      }
    })

    observeEvent(input$type_reseau, {
      req(input$thematique, input$type_reseau)

      # Vérifie si la thématique est Métropole (dans ce cas, pas de liste de noms)
      if (input$thematique == "Métropole") {
        updateSelectInput(session, "nom", choices = c("Sélectionner"))
      } else {
        noms_disponibles <- unique(data[[input$thematique]][[input$type_reseau]]$nom)
        updateSelectInput(session, "nom", choices = c("Sélectionner", noms_disponibles))
      }
    })
  })
}

app_server <- function(input, output, session) {
  mod_dashboard_server("dashboard", data)
}
