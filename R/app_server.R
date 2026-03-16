#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #### Navigation Buttons ####

  hideTab(inputId = "navbar", target = "1. Network Import & Visualisation")
  hideTab(inputId = "navbar", target = "2. Simulation Setup & Run")
  hideTab(inputId = "navbar", target = "3. Simulation Outputs")
  hideTab(inputId = "navbar", target = "4. Download Outputs")
  hideTab(inputId = "navbar", target = "W1. Setup")
  hideTab(inputId = "navbar", target = "W2. Data Entry")
  hideTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download")
  hideTab(inputId = "navbar", target = "Service Distribution Tool")


  # button to go to wizard
  observeEvent(input$intro_goto_wizard, {
    hideTab(inputId = "navbar", target = "1. Network Import & Visualisation")
    hideTab(inputId = "navbar", target = "2. Simulation Setup & Run")
    hideTab(inputId = "navbar", target = "3. Simulation Outputs")
    hideTab(inputId = "navbar", target = "4. Download Outputs")
    hideTab(inputId = "navbar", target = "W1. Setup")
    hideTab(inputId = "navbar", target = "W2. Data Entry")
    hideTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download")
    hideTab(inputId = "navbar", target = "Service Distribution Tool")

    showTab(inputId = "navbar", target = "W1. Setup")

    updateTabsetPanel(session, "navbar",
      selected = "W1. Setup"
    )
  })

 # button to go to simulation tool
  observeEvent(input$intro_goto_sim, {
    hideTab(inputId = "navbar", target = "1. Network Import & Visualisation")
    hideTab(inputId = "navbar", target = "2. Simulation Setup & Run")
    hideTab(inputId = "navbar", target = "3. Simulation Outputs")
    hideTab(inputId = "navbar", target = "4. Download Outputs")
    hideTab(inputId = "navbar", target = "W1. Setup")
    hideTab(inputId = "navbar", target = "W2. Data Entry")
    hideTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download")
    hideTab(inputId = "navbar", target = "Service Distribution Tool")

    showTab(inputId = "navbar", target = "1. Network Import & Visualisation")

    updateTabsetPanel(session, "navbar",
      selected = "1. Network Import & Visualisation"
    )
  })






  #### Wizard Module ####
  wizard <- mod_wizard_server("wizard", session)

  #### Service Distribution Tool Module ####
  mod_distribution_tool_server("distribution_tool")

  #### Network Visualisation Module ####
  network_viz <- mod_network_visualisation_server("network_visualisation", wizard$var, wizard$cal, session)

  #### Simulation Setup & Run Module ####
  sim_out <- mod_simulation_setup_server("simulation_setup", network_viz, session)

  #### Simulation Outputs Module ####
  mod_simulation_outputs_server("simulation_outputs", sim_out, network_viz$viz)

  #### Download Outputs Module ####
  mod_download_outputs_server("download_outputs", sim_out)
}
