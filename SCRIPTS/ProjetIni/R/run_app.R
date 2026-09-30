#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#' @name run_app
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
library(shiny)
library(golem)
library(dplyr)
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  golem_opts = list()  # Ajoutez cet argument pour spécifier golem_opts
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = golem_opts  # Assurez-vous que golem_opts est passé ici
  )
}

