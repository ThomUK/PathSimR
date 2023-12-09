#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...) {
  # from PathSimR. find a better place for this
  options(shiny.maxRequestSize = 30 * 1024^2) # Sets the Shiny file upload size limit to 30MB

  # load the libraries in a single place
  suppressMessages({
    library(DiagrammeR)
    library(magrittr)
    library(readr)
    library(DT)
    library(openxlsx)
    library(grid)
    library(gridExtra)
    library(parallel)
    library(data.table)
    library(tidyverse)
    library(shinyMatrix)
    library(fitdistrplus)
    library(shinyBS)
    library(shinyjs)
    library(shinythemes)
  })

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
