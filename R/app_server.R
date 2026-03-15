#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #### SHINY SERVER CODE (INC SIM CODE) ####

  #### Navigation Buttons ####
  logger::log_debug("Configuring tab hiding and selection behaviour.")

  hideTab(inputId = "navbar", target = "1. Network Import & Visualisation")
  hideTab(inputId = "navbar", target = "2. Simulation Setup & Run")
  hideTab(inputId = "navbar", target = "3. Simulation Outputs")
  hideTab(inputId = "navbar", target = "4. Download Outputs")
  hideTab(inputId = "navbar", target = "W1. Setup")
  hideTab(inputId = "navbar", target = "W2. Data Entry")
  hideTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download")
  hideTab(inputId = "navbar", target = "Service Distribution Tool")



  observeEvent(input$j2w, {
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


  observeEvent(input$j2s1, {
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



  observeEvent(input$j2PSR3, {
    updateTabsetPanel(session, "navbar",
      selected = "3. Simulation Outputs"
    )
  })

  observeEvent(input$jb2niv, {
    updateTabsetPanel(session, "navbar",
      selected = "1. Network Import & Visualisation"
    )
  })



  #### Wizard Module ####
  logger::log_debug("Starting wizard.")
  wizard <- mod_wizard_server("wizard", session)

  #### Service Distribution Tool Module ####
  mod_distribution_tool_server("distribution_tool")

  logger::log_debug("Wizard complete.")
  ###### END OF WIZARD#######
  ###### START OF SIMULATION TOOL##########

  #### Network Visualisation Module ####
  network_viz <- mod_network_visualisation_server("network_visualisation", wizard$var, wizard$cal, session)
  viz <- network_viz$viz





  checklist_viz <- eventReactive(input$checklist, {
    viz()
  })

  output$cl_viz <- DiagrammeR::renderGrViz({
    checklist_viz()
  })




  checklist_table <- eventReactive(input$checklist, {
    # req(input$reps)

    req(input$st)

    if (input$run_type == "Full Simulation") {
      req(input$wu)
      warm_up <- input$wu
    }

    if (input$run_type == "Trial Simulation") {
      warm_up <- 0
    }





    df <- network_viz$inputs()$var_input
    nodes <-
      length(rownames(df[which(rowSums(df[, 1:which(colnames(df) == "serv_dist") -
        1], na.rm = T) != 0), ]))
    # exits<-length(rownames(df[which(rowSums(df[,1:which(colnames(df)=="serv_dist")-1],na.rm = T)==0),]))
    # delay_exits<-length(rownames(df[which(rowSums(df[,1:which(colnames(df)=="serv_dist")-1],na.rm = T)==0&as.character(df$serv_dist_param)!=""),]))

    x <-
      matrix(
        data = c(
          "Simulation Replications",
          "Warm-up Period",
          "Simulation Period",
          "Total Simulation length",
          "Number of Service points",
          ifelse(
            input$run_type == c("Full Simulation"),
            ceiling(input$reps),
            "NA"
          ),
          warm_up,
          input$st,
          warm_up + input$st,
          nodes
        ),
        ncol = 2
      )
    colnames(x) <- c("Metric", "Value")
    x
  })

  output$checklist_table_render <-
    renderTable(
      {
        checklist_table()
      },
      caption = "Checklist",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL)
    )





  observeEvent(input$sim,
    {
      # req(input$file1)
      # req(input$file2)

      if (input$run_type == "Full Simulation") {
        req(input$wu)
      }

      req(input$st)
      req(input$st > 0)
      if (input$run_type == c("Full Simulation")) {
        req(input$reps > 0)
      }

      showModal(modalDialog(
        title = div(paste0("Simulation Running \n (Started at : ", format(Sys.time()), ")"), style = "font-size:200%"),
        div("The simulation is now running. If there is an error, a new message box will appear with advice. Once the simulation is complete, a completion message will appear.", style = "font-size:200%"),
        easyClose = FALSE,
        footer = NULL,
        size = "l"
      ))
    },
    priority = 2
  )



  #### SIMULATION ####
  # sim_out is the object that will contain all the outputs from the simulation and is therefore important in rendering all of the outputs
  # The tryCatch is a error capture system that results in a pop-up for the user if there are any errors within the system. The exact pop-up can be found at the bottom of the simulation section.

  sim_out <- eventReactive(input$sim, {
    logger::log_info("Simulation Started...")
    tryCatch(
      {
        ### Inputs and Initialisation ####

        req(input$st > 0)

        if (input$run_type == c("Full Simulation")) {
          req(input$reps > 0)
        }

        if (input$run_type == c("Trial Simulation")) {
          reps <- 10
        }
        if (input$run_type == c("Full Simulation")) {
          reps <- ceiling(input$reps)
        }

        if (input$run_type == c("Trial Simulation")) {
          warm_up <- 0
        }
        if (input$run_type == c("Full Simulation")) {
          warm_up <- input$wu
        }

        hideTab(inputId = "navbar", target = "3. Simulation Outputs")
        showTab(inputId = "navbar", target = "3. Simulation Outputs")
        hideTab(inputId = "navbar", target = "4. Download Outputs")

        hideTab(inputId = "3. Simulation Outputs", target = "Output Interpretation")

        if (input$run_type == c("Full Simulation")) {
          showTab(inputId = "navbar", target = "4. Download Outputs")
        }

        output$next_button2 <- renderUI({
          column(6, align = "center", actionButton(inputId = "j2PSR3", label = c(tagList(
            "Next", icon("arrow-right")
          ))))
        })

        ##### Simulation Inputs ##############################################################
        logger::log_trace("Sim inputs.")
        inp <- network_viz$inputs()
        var_input <- inp$var_input
        cal_input <- inp$cal_input

        syst_names <-
          cbind(as.numeric(seq_len(nrow(var_input))), as.character(rownames(var_input)))
        syst_names_single <- syst_names[, 2]

        rownames(var_input) <- seq_len(nrow(var_input))
        colnames(var_input)[seq_len(nrow(var_input))] <- seq_len(nrow(var_input))
        cal_input$node <- as.character(cal_input$node)

        ##### Run Simulation ##############################################################
        logger::log_info("Calling run_simulation().")
        run_simulation(
          var_input = var_input,
          cal_input = cal_input,
          sim_time = input$st,
          warm_up = warm_up,
          reps = reps,
          syst_names = syst_names,
          syst_names_single = syst_names_single,
          time_unit = network_viz$time_unit(),
          session = session
        )
      },
      error = function(e) {
        logger::log_error("Simulation error: ", conditionMessage(e))
        traceback()

        showModal(modalDialog(
          title = div("Simulation Error", style = "font-size:200%"),
          div(
            "Try running the simulation for longer (increase simulation \n
            period length). If the error persists, return to the data input \n
            pages and check that data has been entered correctly. \n
            Click anywhere on screen to continue.",
            style = "font-size:200%"
          ),
          div("Error message: ", conditionMessage(e)),
          easyClose = TRUE,
          footer = NULL,
          size = "l"
        ))

        hideTab(inputId = "navbar", target = "3. Simulation Outputs")
        hideTab(inputId = "navbar", target = "4. Download Outputs")

        stop(e)
        return(NULL)
      }
    )
  }) # END OF sim_out() FUNCTION ####





  ### OUTPUT RENDER TEXT ####
  output$comp <- renderText({
    req(sim_out())
    x <- sim_out()
    y <- x$reps
    time <- proc.time() - x$ptm
    p <-
      paste0(
        "Simulation completed in ",
        round(time[3], digits = 1),
        " seconds."
      )
    logger::log_info(p)
    p
  })


  # output$run_time<-renderTable({
  #   req(sim_out())
  #   out<-sim_out()
  #   x<-out$reps
  #   time<-proc.time()-out$ptm
  #   rep_run<-time[3]/x
  #
  #   y<-matrix(data = c("10 runs","100 runs","500 runs","1,000 runs","10,000 runs",
  #                      round(10*rep_run/60,digits=2),round(100*rep_run/60,digits=2),round(500*rep_run/60,digits=2),round(1000*rep_run/60,digits=2),round(10000*rep_run/60,digits=2),
  #                      round(10*rep_run/3600,digits=2),round(100*rep_run/3600,digits=2),round(500*rep_run/3600,digits=2),round(1000*rep_run/3600,digits=2),round(10000*rep_run/3600,digits=2)),
  #             ncol=3)
  #
  #   colnames(y)<-c("# of replicates","Run time (in minutes)","Run time (in hours)")
  #   y
  #
  #
  # },caption = "Run Time Estimates",
  # caption.placement = getOption("xtable.caption.placement", "top"),
  # caption.width = getOption("xtable.caption.width", NULL))
  #

  #### Simulation Outputs Module ####
  mod_simulation_outputs_server("simulation_outputs", sim_out, viz)


  #### Download Outputs Module ####
  mod_download_outputs_server("download_outputs", sim_out)
}
