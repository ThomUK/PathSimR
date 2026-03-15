#' Simulation Setup & Run module server (Tab 2)
#'
#' @param id Module id string.
#' @param network_viz Return value from `mod_network_visualisation_server()`:
#'   a list with `$inputs()`, `$viz`, and `$time_unit()`.
#' @param parent_session The session object from `app_server`, needed for
#'   `updateTabsetPanel()` and `showTab()`/`hideTab()` on the global navbar.
#'
#' @returns An eventReactive (`sim_out`) containing the full simulation output list.
#' @noRd
mod_simulation_setup_server <- function(id, network_viz, parent_session) {
  moduleServer(id, function(input, output, session) {
    #### Navigation ####

    observeEvent(input$sim_prev, {
      updateTabsetPanel(parent_session, "navbar",
        selected = "1. Network Import & Visualisation"
      )
    })

    observeEvent(input$sim_next, {
      updateTabsetPanel(parent_session, "navbar",
        selected = "3. Simulation Outputs"
      )
    })

    #### Checklist ####

    checklist_viz <- eventReactive(input$checklist, {
      network_viz$viz()
    })

    output$cl_viz <- DiagrammeR::renderGrViz({
      checklist_viz()
    })

    checklist_table <- eventReactive(input$checklist, {
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

    #### Simulation ####

    observeEvent(input$sim,
      {
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

    sim_out <- eventReactive(input$sim, {
      logger::log_info("Simulation Started...")
      tryCatch(
        {
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

          hideTab(inputId = "navbar", target = "3. Simulation Outputs", session = parent_session)
          showTab(inputId = "navbar", target = "3. Simulation Outputs", session = parent_session)
          hideTab(inputId = "navbar", target = "4. Download Outputs", session = parent_session)

          hideTab(inputId = "3. Simulation Outputs", target = "Output Interpretation", session = parent_session)

          if (input$run_type == c("Full Simulation")) {
            showTab(inputId = "navbar", target = "4. Download Outputs", session = parent_session)
          }

          output$next_button2 <- renderUI({
            column(6, align = "center", actionButton(inputId = session$ns("sim_next"), label = c(tagList(
              "Next", icon("arrow-right")
            ))))
          })

          inp <- network_viz$inputs()
          var_input <- inp$var_input
          cal_input <- inp$cal_input

          syst_names <-
            cbind(as.numeric(seq_len(nrow(var_input))), as.character(rownames(var_input)))
          syst_names_single <- syst_names[, 2]

          rownames(var_input) <- seq_len(nrow(var_input))
          colnames(var_input)[seq_len(nrow(var_input))] <- seq_len(nrow(var_input))
          cal_input$node <- as.character(cal_input$node)

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
            session = parent_session
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

          hideTab(inputId = "navbar", target = "3. Simulation Outputs", session = parent_session)
          hideTab(inputId = "navbar", target = "4. Download Outputs", session = parent_session)

          stop(e)
          return(NULL)
        }
      )
    })

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

    sim_out
  })
}
