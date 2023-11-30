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
      theme = shinytheme("cerulean"),
      title = c(tagList(icon("compass"), "Navigation Bar")),
      id = "navbar",

      #### INTRODUCTION TAB ####
      ui_tab_introduction(),

      #### OVERVIEW AND GLOSSARY TAB ####
      ui_tab_overview_and_glossary(),

      #### WIZARD 1 - SETUP TAB ####
      ui_tab_wizard_1(),

      #### WIZARD 2 - DATA ENTRY TAB ####
      ui_tab_wizard_2(),

      #### WIZARD 3 - FINAL WIZARD TABLES & DOWNLOAD TAB ####
      ui_tab_wizard_3(),

      #### SERVICE DISTRIBUTION TOOL TAB ####
      ui_tab_service_dist(),

      #### TOOL 1 - NETWORK IMPORT & VISUALISATION TAB ####
      ui_tab_tool_1(),

      #### TOOL 2 - SIMULATION SETUP & RUN TAB ####
      ui_tab_tool_2(),

      #### TOOL 3 - SIMULATION OUTPUTS TAB ####
      ui_tab_tool_3(),

      #### TOOL 4 - DOWNLOAD OUTPUTS TAB ####
      ui_tab_tool_4()
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
