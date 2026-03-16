#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(

      ##### SHINY UI CODE #####
      theme = shinythemes::shinytheme("cerulean"),
      title = c(tagList(icon("compass"), "Navigation Bar")),
      id = "navbar",

      #### INTRODUCTION TAB ####
      ui_tab_introduction(),

      #### OVERVIEW AND GLOSSARY TAB ####
      ui_tab_overview_and_glossary(),

      #### WIZARD 1 - SETUP TAB ####
      mod_wizard_ui_1("wizard"),

      #### WIZARD 2 - DATA ENTRY TAB ####
      mod_wizard_ui_2("wizard"),

      #### WIZARD 3 - FINAL WIZARD TABLES & DOWNLOAD TAB ####
      mod_wizard_ui_3("wizard"),

      #### SERVICE DISTRIBUTION TOOL TAB ####
      mod_distribution_tool_ui("distribution_tool"),

      #### TOOL 1 - NETWORK IMPORT & VISUALISATION TAB ####
      mod_network_visualisation_ui("network_visualisation"),

      #### TOOL 2 - SIMULATION SETUP & RUN TAB ####
      mod_simulation_setup_ui("simulation_setup"),

      #### TOOL 3 - SIMULATION OUTPUTS TAB ####
      mod_simulation_outputs_ui("simulation_outputs"),

      #### TOOL 4 - DOWNLOAD OUTPUTS TAB ####
      mod_download_outputs_ui("download_outputs")
    )
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
      app_title = "PathSimR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
