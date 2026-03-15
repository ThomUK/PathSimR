#' Wizard module server
#'
#' @param id Module id string, must match the id used in the three UI functions.
#' @param parent_session The session object from `app_server`, needed so that
#'   navigation buttons inside the wizard can update the top-level navbar.
#'
#' @returns A list with two eventReactives: `var` and `cal`.
#' @noRd
mod_wizard_server <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    #### Figures for Modals ####
    output$ext_arr_example <- renderTable(
      {
        data.frame(
          "Start Time" = c(0, 100, 150),
          "End Time" = c(100, 150, 200),
          "Arrival Rate" = c(1, 0, 2)
        )
      },
      caption = "Example External Arrival Calendar",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL),
      striped = TRUE,
      bordered = TRUE
    )

    output$cap_example <- renderTable(
      {
        data.frame(
          "Start Time" = c(0, 30, 90),
          "End Time" = c(30, 90, 180),
          "Capacity" = c(24, 48, 72)
        )
      },
      caption = "Example Capacity Calendar",
      caption.placement = getOption("xtable.caption.placement", "top"),
      caption.width = getOption("xtable.caption.width", NULL),
      striped = TRUE,
      bordered = TRUE
    )

    #### Navigation Buttons ####

    observeEvent(input$jb2i2, {
      updateTabsetPanel(parent_session, "navbar",
        selected = "Introduction"
      )
    })

    observeEvent(input$j2de, {
      showTab(inputId = "navbar", target = "W2. Data Entry", session = parent_session)
      showTab(inputId = "navbar", target = "Service Distribution Tool", session = parent_session)
      updateTabsetPanel(parent_session, "navbar",
        selected = "W2. Data Entry"
      )
    })

    observeEvent(input$j2s, {
      updateTabsetPanel(parent_session, "navbar",
        selected = "W1. Setup"
      )
    })

    observeEvent(input$j2ftd, {
      showTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download", session = parent_session)
      updateTabsetPanel(parent_session, "navbar",
        selected = "W3. Final Wizard Tables & Download"
      )
    })

    observeEvent(input$jb2de, {
      showTab(inputId = "navbar", target = "W2. Data Entry", session = parent_session)
      updateTabsetPanel(parent_session, "navbar",
        selected = "W2. Data Entry"
      )
    })

    observeEvent(input$j2PSR, {
      showTab(inputId = "navbar", target = "1. Network Import & Visualisation", session = parent_session)
      updateTabsetPanel(parent_session, "navbar",
        selected = "1. Network Import & Visualisation"
      )
    })

    #### Name Input tables and checks ####
    ### Creates table of service point names ###
    output$sp_table <- renderTable(
      {
        x <- input$service_points
        x <- unique(x)
        rownames(x) <- 1:nrow(x)
        x <- trimws(x = x, which = "both")
        x <- gsub(x = x, pattern = " ", "_")
        x <- x[which(x != "")]
        x <- data.frame("Service Points" = x)
        colnames(x) <- "Service Points" # reinstate the space
        x
      },
      rownames = FALSE,
      striped = TRUE,
      bordered = TRUE
    )


    ### Creates table of exit names ###
    output$exit_table <- renderTable(
      {
        x <- input$exits
        x <- unique(x)
        rownames(x) <- 1:nrow(x)
        x <- trimws(x = x, which = "both")
        x <- gsub(x = x, pattern = " ", "_")
        x <- x[which(x != "")]
        data.frame("Exits" = x)
      },
      rownames = FALSE,
      striped = TRUE,
      bordered = TRUE
    )



    ### Creates text for duplicates ###
    output$duplicate <- renderText({
      x <- input$service_points
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      colnames(x) <- "Service Point"
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      x <- x[which(x != "")]
      x <- data.frame("Service Points" = x)
      colnames(x) <- "Service Points"
      s <- x



      x <- input$exits
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      x <- x[which(x != "")]
      e <- data.frame("Exits" = x)

      if (any(s[, 1] %in% e[, 1]) | any(e[, 1] %in% s[, 1])) {
        "One or more names appear in both the Service Point & Exit lists. \n Please update before proceeding."
      }
    })

    #### Creates the Data Entry Service Point tabs UI ####
    output$tabs <- renderUI({
      x <- input$service_points
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      sp <- x[which(x != "")]

      x <- input$exits
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      exit <- x[which(x != "")]

      node_number <- length(sp)
      exit_number <- length(exit)
      tabnames <- sp

      node_names <- sp

      exit_names <- exit

      all_names <- c(node_names, exit_names)



      #### Creates the transition probability inputs & delay departure entry (dynamic based on number of nodes & exits) ####
      logger::log_debug("Creating transition probability and delay departures.")
      for (j in 1:node_number) {
        assign(
          x = paste0("transition_", j),
          value = lapply(1:length(all_names), function(i) {
            if (j != i) {
              column(
                2,
                fluidRow(
                  column(
                    12,
                    align = "center",
                    h4(all_names[i]),
                    style = "padding:2px; font-size:150%"
                  )
                ),
                numericInput(
                  inputId = session$ns(paste0("transition_", j, "_", i)),
                  label = paste("Proportion from", all_names[j], "to", all_names[i]),
                  value = 0,
                  min = 0,
                  max = 1,
                  step = 0.001
                ),
                selectInput(
                  inputId = session$ns(paste0("delay_dist_", j, "_", i)),
                  label = paste(
                    "Distribution for Transition Delay from",
                    all_names[j],
                    "to",
                    all_names[i]
                  ),
                  choices = c(
                    "None",
                    "Exponential",
                    "log-Normal",
                    "Uniform",
                    "Weibull",
                    "Gamma"
                  ),
                  selected = "None",
                  selectize = F
                ),
                fluidRow(
                  conditionalPanel(
                    condition = paste0(
                      "input['",
                      session$ns(paste0("delay_dist_", j, "_", i)),
                      "'] == 'None'"
                    ),
                    shinyjs::disabled(column(
                      12,
                      textInput(
                        inputId = session$ns(paste0("delay_param_none_1_", i)),
                        value = "NA",
                        label = "No Parameters Required"
                      )
                    ))
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input['",
                      session$ns(paste0("delay_dist_", j, "_", i)),
                      "'] == 'Exponential'"
                    ),
                    column(
                      6,
                      numericInput(
                        inputId = session$ns(paste0("delay_param_exp_1_", j, "_", i)),
                        label = "Rate",
                        value = "",
                        min = 0,
                        step = 0.0001
                      )
                    ),
                    column(6, br())
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input['",
                      session$ns(paste0("delay_dist_", j, "_", i)),
                      "'] == 'log-Normal'"
                    ),
                    column(
                      6,
                      numericInput(
                        inputId = session$ns(paste0("delay_param_lnorm_1_", j, "_", i)),
                        label = "meanlog",
                        value = "",
                        step = 0.0001
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        inputId = session$ns(paste0("delay_param_lnorm_2_", j, "_", i)),
                        label = "sdlog",
                        value = "",
                        step = 0.0001
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input['",
                      session$ns(paste0("delay_dist_", j, "_", i)),
                      "'] == 'Uniform'"
                    ),
                    column(
                      6,
                      numericInput(
                        inputId = session$ns(paste0("delay_param_unif_1_", j, "_", i)),
                        label = "Min",
                        value = "",
                        min = 0,
                        step = 0.0001
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        inputId = session$ns(paste0("delay_param_unif_2_", j, "_", i)),
                        label = "Max",
                        value = "",
                        min = 0,
                        step = 0.0001
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input['",
                      session$ns(paste0("delay_dist_", j, "_", i)),
                      "'] == 'Weibull'"
                    ),
                    column(
                      6,
                      numericInput(
                        inputId = session$ns(paste0("delay_param_weibull_1_", j, "_", i)),
                        label = "Shape",
                        value = "",
                        step = 0.0001
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        inputId = session$ns(paste0("delay_param_weibull_2_", j, "_", i)),
                        label = "Scale",
                        value = "",
                        step = 0.0001
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = paste0(
                      "input['",
                      session$ns(paste0("delay_dist_", j, "_", i)),
                      "'] == 'Gamma'"
                    ),
                    column(
                      6,
                      numericInput(
                        inputId = session$ns(paste0("delay_param_gamma_1_", j, "_", i)),
                        label = "Shape",
                        value = "",
                        step = 0.001
                      )
                    ),
                    column(
                      6,
                      numericInput(
                        inputId = session$ns(paste0("delay_param_gamma_2_", j, "_", i)),
                        label = "Rate",
                        value = "",
                        step = 0.001
                      )
                    )
                  )
                ),
                br(),
                style = "border:0.5px dashed #e6e6e6;"
              )
            } else {
              column(
                2,
                fluidRow(
                  column(
                    12,
                    align = "center",
                    h4(all_names[i]),
                    style = "padding:2px; font-size:150%"
                  )
                ),
                shinyjs::disabled(
                  numericInput(
                    inputId = session$ns(paste0("transition_", j, "_", i)),
                    label = paste("Proportion from", all_names[j], "to", all_names[i]),
                    value = 0
                  )
                ),
                shinyjs::disabled(
                  textInput(
                    inputId = session$ns(paste0("delay_dist_", j, "_", i)),
                    label = paste(
                      "Distribution for Transition Delay from",
                      all_names[j],
                      "to",
                      all_names[i]
                    ),
                    value = "None"
                  )
                ),
                fluidRow(column(12, shinyjs::disabled(
                  textInput(
                    inputId = session$ns(paste0("delay_param_none_1_", i)),
                    value = "NA",
                    label = "No Parameters Required"
                  )
                ))),
                br(),
                style = "border:0.5px dashed #e6e6e6;"
              )
            }
          })
        )
      }




      arrivals_calendar <- matrix(
        ncol = 3,
        nrow = 1,
        data = c(0, "", "")
      )
      colnames(arrivals_calendar) <- c("Start Time", "End Time", "Arrival Rate")

      capacity_calendar <- matrix(
        ncol = 3,
        nrow = 1,
        data = c(0, "", "")
      )
      colnames(capacity_calendar) <- c("Start Time", "End Time", "Capacity")



      #### Defines 'tabs' layout (dynamic based on number of nodes & exits) ####
      myTabs <- lapply(1:node_number, function(i) {
        tabPanel(
          title = HTML(tabnames[i]),
          shinyjs::useShinyjs(),
          br(),
          h1(paste("Service Point Name:", tabnames[i])),
          hr(),
          h4("Length of Service Information"),
          # p("If distribution and parameters for the service point are not know, use the Service Distribution Tool (in the bar above) to either fit models to uploaded data or scale against BNSSG data and then enter resulting distributions and parameters.", style="color:gray"),

          fluidRow(
            column(
              2,
              selectInput(
                inputId = session$ns(paste0("serv_dist_", i)),
                label = "Select a distribution",
                choices = c(
                  "Exponential",
                  "log-Normal",
                  "Uniform",
                  "Weibull",
                  "Gamma"
                ),
                selected = "Exponential"
              ),
              selectize = F
            ),
            column(
              width = 1,
              offset = 0,
              style = "padding:0px;"
            ),
            conditionalPanel(
              condition = paste0("input['", session$ns(paste0("serv_dist_", i)), "'] == 'Exponential'"),
              column(
                2,
                numericInput(
                  inputId = session$ns(paste0("serv_param_exp_1_", i)),
                  label = "Rate",
                  value = "",
                  step = 0.001,
                  min = 0
                )
              )
            ),
            conditionalPanel(
              condition = paste0("input['", session$ns(paste0("serv_dist_", i)), "'] == 'log-Normal'"),
              column(
                2,
                numericInput(
                  inputId = session$ns(paste0("serv_param_lnorm_1_", i)),
                  label = "meanlog",
                  value = "",
                  step = 0.001
                )
              ),
              column(
                2,
                numericInput(
                  inputId = session$ns(paste0("serv_param_lnorm_2_", i)),
                  label = "sdlog",
                  value = "",
                  step = 0.001
                )
              )
            ),
            conditionalPanel(
              condition = paste0("input['", session$ns(paste0("serv_dist_", i)), "'] == 'Uniform'"),
              column(
                2,
                numericInput(
                  inputId = session$ns(paste0("serv_param_unif_1_", i)),
                  label = "Min",
                  value = "",
                  step = 0.001,
                  min = 0
                )
              ),
              column(
                2,
                numericInput(
                  inputId = session$ns(paste0("serv_param_unif_2_", i)),
                  label = "Max",
                  value = "",
                  step = 0.001,
                  min = 0
                )
              )
            ),
            conditionalPanel(
              condition = paste0("input['", session$ns(paste0("serv_dist_", i)), "'] == 'Weibull'"),
              column(
                2,
                numericInput(
                  inputId = session$ns(paste0("serv_param_weibull_1_", i)),
                  label = "Shape",
                  value = "",
                  step = 0.001
                )
              ),
              column(
                2,
                numericInput(
                  inputId = session$ns(paste0("serv_param_weibull_2_", i)),
                  label = "Scale",
                  value = "",
                  step = 0.001
                )
              )
            ),
            conditionalPanel(
              condition = paste0("input['", session$ns(paste0("serv_dist_", i)), "'] == 'Gamma'"),
              column(
                2,
                numericInput(
                  inputId = session$ns(paste0("serv_param_gamma_1_", i)),
                  label = "Shape",
                  value = "",
                  step = 0.001
                )
              ),
              column(
                2,
                numericInput(
                  inputId = session$ns(paste0("serv_param_gamma_2_", i)),
                  label = "Rate",
                  value = "",
                  step = 0.001
                )
              )
            ),
            style = "border-bottom:1px dashed silver;"
          ),
          h4("Queue Information"),
          # p("An external queue is defined as a queue that accepts arrivals from outside of the pathway.
          # An internal queue is one that connects service points within the pathway network.", style="color:gray"),
          fluidRow(
            column(
              2,
              numericInput(
                inputId = session$ns(paste0("ext_q_", i)),
                label = "External Queue Capacity",
                value = 9999,
                min = 0
              )
            ),
            column(
              width = 1,
              offset = 0,
              style = "padding:0px;"
            ),
            column(
              2,
              numericInput(
                inputId = session$ns(paste0("int_q_", i)),
                label = "Internal Queue Capacity",
                value = 0,
                min = 0
              )
            ),
            style = "border-bottom:1px dashed silver;"
          ),
          fluidRow(column(
            12,
            h4("Transitions & Departure Delays"),
            # p("All proportion values should sum to 1 for each Service Point tab.
            #   A fixed presscribed delay can be modelled using the uniform distribution and entering the same value into the min and max boxes.", style="color:gray"),
            do.call(fluidRow, get(paste0(
              "transition_", i
            )))
          )),
          fluidRow(
            column(
              width = 12,
              offset = 0,
              style = "padding:10px;"
            ),
            style = "border-bottom:1px dashed silver;"
          ),
          fluidRow(
            column(
              5,
              h4("External Arrival Rate Calendar"),
              p(
                "For more information, consult the instructions sidebar (Step 4) and 'How do I fill the calendar?' info button",
                style = "color:gray"
              ),
              fluidRow(column(
                12,
                shinyMatrix::matrixInput(
                  inputId = session$ns(paste0("ext_arr_", i)),
                  value = arrivals_calendar,
                  class = "numeric",
                  cols = list(
                    names = TRUE,
                    extend = FALSE,
                    editableNames = FALSE
                  ),
                  rows = list(
                    names = FALSE,
                    extend = TRUE,
                    editableNames = FALSE,
                    delta = 1
                  ),
                  copy = FALSE,
                  paste = TRUE
                )
              ))
            ),
            column(
              width = 1,
              offset = 0,
              style = "padding:0px;"
            ),
            column(
              5,
              h4("Capacity Calendar"),
              p(
                "For more information, consult the instructions sidebar (Step 4) and 'How to fill out the calendar?' info button",
                style = "color:gray"
              ),
              fluidRow(column(
                12,
                shinyMatrix::matrixInput(
                  inputId = session$ns(paste0("cap_", i)),
                  value = capacity_calendar,
                  class = "numeric",
                  cols = list(
                    names = TRUE,
                    extend = FALSE,
                    editableNames = FALSE
                  ),
                  rows = list(
                    names = FALSE,
                    extend = TRUE,
                    editableNames = FALSE,
                    delta = 1
                  ),
                  copy = FALSE,
                  paste = TRUE
                )
              ))
            ),
            column(
              width = 1,
              offset = 0,
              style = "padding:0px;"
            )
          )
        )
      })
      do.call(tabsetPanel, myTabs)
    })




    #### Creates the trial Var_input ####
    var <- eventReactive(input$go, {
      x <- input$service_points
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      sp <- x[which(x != "")]

      x <- input$exits
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      exit <- x[which(x != "")]

      node_number <- length(sp)
      exit_number <- length(exit)
      tabnames <- sp

      node_names <- sp

      exit_names <- exit

      all_names <- c(node_names, exit_names)




      ## Transition Matrix
      t1 <- lapply(1:node_number, function(j) {
        t2 <- lapply(1:length(all_names), function(i) {
          input[[paste0("transition_", j, "_", i)]]
        })
        t2
      })

      dat <- as.numeric(as.vector(unlist(t1)))

      tm <- matrix(
        data = dat,
        nrow = length(node_names),
        byrow = T
      )
      # tm<-t(tm)
      colnames(tm) <- all_names
      rownames(tm) <- node_names


      ## Queues

      iq <- lapply(1:node_number, function(i) {
        input[[paste0("int_q_", i)]]
      })
      iq <- as.numeric(as.vector(unlist(iq)))


      eq <- lapply(1:node_number, function(i) {
        input[[paste0("ext_q_", i)]]
      })
      eq <- as.numeric(as.vector(unlist(eq)))

      ## Service Distributions & Parameters

      sd <- lapply(1:node_number, function(i) {
        input[[paste0("serv_dist_", i)]]
      })
      sd <- as.vector(unlist(sd))


      sdp <- lapply(1:node_number, function(i) {
        if (sd[i] == "Exponential") {
          input[[paste0("serv_param_exp_1_", i)]]
        } else if (sd[i] == "log-Normal") {
          paste0(input[[paste0("serv_param_lnorm_1_", i)]], ";", input[[paste0("serv_param_lnorm_2_", i)]])
        } else if (sd[i] == "Uniform") {
          paste0(input[[paste0("serv_param_unif_1_", i)]], ";", input[[paste0("serv_param_unif_2_", i)]])
        } else if (sd[i] == "Weibull") {
          paste0(input[[paste0("serv_param_weibull_1_", i)]], ";", input[[paste0("serv_param_weibull_2_", i)]])
        } else if (sd[i] == "Gamma") {
          paste0(input[[paste0("serv_param_gamma_1_", i)]], ";", input[[paste0("serv_param_gamma_2_", i)]])
        }
      })
      sdp <- as.vector(unlist(sdp))



      ## Delay Distribution Matrix

      dd1 <- lapply(1:node_number, function(j) {
        dd2 <- lapply(1:length(all_names), function(i) {
          input[[paste0("delay_dist_", j, "_", i)]]
        })
        dd2
      })



      dat <- as.vector(unlist(dd1))

      ddm <- matrix(
        data = dat,
        nrow = length(node_names),
        byrow = T
      )

      ddm[which(ddm == "None")] <- NA
      ddm[which(ddm == "Exponential")] <- "exp"
      ddm[which(ddm == "log-Normal")] <- "lnorm"
      ddm[which(ddm == "Uniform")] <- "unif"
      ddm[which(ddm == "Weibull")] <- "weibull"
      ddm[which(ddm == "Gamma")] <- "gamma"



      # tm<-t(tm)
      colnames(ddm) <- all_names
      rownames(ddm) <- paste0(node_names, "_Delay_Dist")
      output$ddm <- renderTable({
        ddm
      })




      ## Delay Parameter Matrix

      dp1 <- lapply(1:node_number, function(j) {
        dp2 <- lapply(1:length(all_names), function(i) {
          test <- input[[paste0("delay_dist_", j, "_", i)]]


          if (test == "None") {
            NA
          } else if (test == "Exponential") {
            input[[paste0("delay_param_exp_1_", j, "_", i)]]
          } else if (test == "log-Normal") {
            paste0(input[[paste0("delay_param_lnorm_1_", j, "_", i)]], ";", input[[paste0("delay_param_lnorm_2_", j, "_", i)]])
          } else if (test == "Uniform") {
            paste0(input[[paste0("delay_param_unif_1_", j, "_", i)]], ";", input[[paste0("delay_param_unif_2_", j, "_", i)]])
          } else if (test == "Weibull") {
            paste0(input[[paste0("delay_param_weibull_1_", j, "_", i)]], ";", input[[paste0("delay_param_weibull_2_", j, "_", i)]])
          } else if (test == "Gamma") {
            paste0(input[[paste0("delay_param_gamma_1_", j, "_", i)]], ";", input[[paste0("delay_param_gamma_2_", j, "_", i)]])
          }
        })
        dp2
      })

      ddp <- as.vector(unlist(dp1))

      ddp <- matrix(
        data = ddp,
        nrow = length(node_names),
        byrow = T
      )



      colnames(ddp) <- all_names
      rownames(ddp) <- paste0(node_names, "_Delay_Params")


      ####


      var <- cbind(tm, sd, sdp, eq, iq, ddm, ddp)
      var <- rbind(var, matrix(NA, nrow = exit_number, ncol = ncol(var)))

      rownames(var) <- all_names
      colnames(var) <-
        c(
          all_names,
          "serv_dist",
          "serv_dist_param",
          "ext_queue",
          "int_queue",
          paste0(all_names, "_delay_dist"),
          paste0(all_names, "_delay_params")
        )

      var <- as.data.frame(var)

      var$serv_dist <-
        gsub(
          x = var$serv_dist,
          pattern = "Exponential",
          replacement = "exp"
        )
      var$serv_dist <-
        gsub(
          x = var$serv_dist,
          pattern = "log-Normal",
          replacement = "lnorm"
        )
      var$serv_dist <-
        gsub(
          x = var$serv_dist,
          pattern = "Uniform",
          replacement = "unif"
        )
      var$serv_dist <-
        gsub(
          x = var$serv_dist,
          pattern = "Weibull",
          replacement = "weibull"
        )
      var$serv_dist <-
        gsub(
          x = var$serv_dist,
          pattern = "Gamma",
          replacement = "gamma"
        )

      var
    })


    #### Creates the trial Cal_input ####
    cal <- eventReactive(input$go, {
      x <- input$service_points
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      sp <- x[which(x != "")]

      x <- input$exits
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      exit <- x[which(x != "")]

      node_number <- length(sp)
      exit_number <- length(exit)
      tabnames <- sp

      node_names <- sp

      exit_names <- exit

      all_names <- c(node_names, exit_names)

      ## External Arrival
      ea <- lapply(1:node_number, function(i) {
        x <- as.data.frame(input[[paste0("ext_arr_", i)]])
        # x <- head(x, -1)
        if (nrow(x) > 0) {
          x <- cbind("ext_arr", paste0(node_names[i]), x)
          colnames(x) <- c("metric", "node", "start", "end", "value")
        }
        x
      })

      ea_rows <- lapply(1:node_number, function(i) {
        nrow(ea[[i]])
      })

      if (!all(ea_rows == 0)) {
        eam <- data.table::rbindlist(ea[c(which(ea_rows > 0))])
      }




      ## Capacity
      cap <- lapply(1:node_number, function(i) {
        x <- as.data.frame(input[[paste0("cap_", i)]])
        # x <- head(x, -1)
        if (nrow(x) > 0) {
          x <- cbind("cap", paste0(node_names[i]), x)
          colnames(x) <- c("metric", "node", "start", "end", "value")
        }
        x
      })

      cap_rows <- lapply(1:node_number, function(i) {
        nrow(cap[[i]])
      })

      if (!all(cap_rows == 0)) {
        capm <- data.table::rbindlist(cap[c(which(cap_rows > 0))])
      }


      if (exists("eam") & exists("capm")) {
        cal <- rbind(eam, capm)
        colnames(cal) <- c("metric", "node", "start", "end", "value")
        cal
      } else if (exists("eam")) {
        cal <- eam
        colnames(cal) <- c("metric", "node", "start", "end", "value")
        cal
      } else if (exists("capm")) {
        cal <- capm
        colnames(cal) <- c("metric", "node", "start", "end", "value")
        cal
      } else {
        cal <-
          data.frame(
            "metric" = "",
            "node" = "",
            "start" = "",
            "end" = "",
            "value" = ""
          )
        cal
      }
    })

    #### Creates the Var_input visual####
    observeEvent(input$go, {
      output$var_view <- renderTable(
        {
          var()
        },
        rownames = TRUE,
        striped = T,
        bordered = T,
        align = "c",
        caption = "Network Information",
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL)
      )
    })


    #### Creates the Cal_input visual####
    observeEvent(input$go, {
      output$cal_view <- renderTable(
        {
          cal()
        },
        rownames = FALSE,
        striped = T,
        bordered = T,
        align = "c",
        caption = "Calendar Information",
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL),
        digits = 5
      )
    })

    #### Creates the input checklist####
    logger::log_debug("Creating input checklist.")
    observeEvent(input$go, {
      issues <- c()

      var <- var()
      cal <- cal()

      x <- input$service_points
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      sp <- x[which(x != "")]

      x <- input$exits
      x <- unique(x)
      rownames(x) <- 1:nrow(x)
      x <- trimws(x = x, which = "both")
      x <- gsub(x = x, pattern = " ", "_")
      exit <- x[which(x != "")]

      node_number <- length(sp)
      exit_number <- length(exit)
      tabnames <- sp

      node_names <- sp

      exit_names <- exit

      all_names <- c(node_names, exit_names)



      ### Testing if the transition matrix has rowsums of 1###

      f <- var[1:node_number, 1:length(all_names)]
      indx <- sapply(f, is.character)
      f[indx] <-
        lapply(f[indx], function(x) {
          as.numeric(as.character(x))
        })
      transition <- as.data.frame(f)



      if (sum(transition < 0) > 0 | sum(transition > 1) > 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            "All",
            paste(
              "Transition proportions contains value outside required range (replace with value between 0 and 1)",
              sep = ""
            )
          ))
      }



      rs <- rowSums(transition)

      for (i in 1:node_number) {
        x <- rs[i]

        if (is.na(x)) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste(
                "Transition row contains NA (replace with 0 or value)",
                sep = ""
              )
            ))
        } else if (!isTRUE(dplyr::near(x, 1))) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste(
                "Transition proportion row does not sum to 1 (Currently:",
                x,
                ")",
                sep = ""
              )
            ))
        }
      }


      ### Testing if the distribution parameter inputs are correct ###
      f <-
        var[1:node_number, (length(all_names) + 1):(length(all_names) + 2)]
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      serv_dist_param <- as.data.frame(f)



      for (i in 1:node_number) {
        if (serv_dist_param[i, 1] == "exp") {
          x <- serv_dist_param[i, 2]

          if (is.na(x)) {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Missing a service distribution parameter"
              )
          }

          if ((!is.na(x)) & x <= 0) {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Service distribution parameter is not greater than 0"
              )
          }
        } else {
          x <- serv_dist_param[i, 2]
          x <- strsplit(x, ";")[[1]]

          if ("NA" %in% x) {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Missing a service distribution parameter"
              )
          }

          if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "unif") {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Service distribution parameters are not greater than 0"
              )
          }

          if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "gamma") {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Service distribution parameters are not greater than 0"
              )
          }

          if ((!("NA" %in% x)) &
            any(x <= 0) & serv_dist_param[i, 1] == "weibull") {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Service distribution parameters are not greater than 0"
              )
          }

          if ((!("NA" %in% x)) & x[2] < 0 &
            serv_dist_param[i, 1] == "lnorm") {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "lnorm service parameter (sdlog) is less than 0"
              )
          }
        }
      }


      #### Testing if the Queue inputs are correct #
      iq <- lapply(1:node_number, function(i) {
        input[[paste0("int_q_", i)]]
      })
      iq <- as.numeric(as.vector(unlist(iq)))


      eq <- lapply(1:node_number, function(i) {
        input[[paste0("ext_q_", i)]]
      })
      eq <- as.numeric(as.vector(unlist(eq)))

      for (i in 1:node_number) {
        x <- iq[i]

        if (is.na(x)) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter Internal Queue Value")
            ))
        }

        if (x %% 1 != 0) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter an integer Internal Queue Value")
            ))
        }

        if (x < 0) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter a positive Internal Queue Value")
            ))
        }
      }

      for (i in 1:node_number) {
        x <- eq[i]

        if (is.na(x)) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter External Queue Value")
            ))
        }

        if (x %% 1 != 0) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter an integer External Queue Value")
            ))
        }

        if (x < 0) {
          issues <-
            c(issues, c(
              paste0("Network Input"),
              node_names[i],
              paste("Need to enter a positive External Queue Value")
            ))
        }
      }



      ### Testing if the delay parameter inputs are correct ###

      f <-
        var[1:node_number, (length(all_names) + 5):((2 * length(all_names)) + 4)]
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      delay_dist <- as.data.frame(f)


      f <- var[1:node_number, (2 * length(all_names) + 5):ncol(var)]
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      delay_param <- as.data.frame(f)

      for (j in 1:length(all_names)) {
        for (i in 1:node_number) {
          if (!is.na(delay_dist[i, j])) {
            if (delay_dist[i, j] == "exp") {
              x <- delay_param[i, j]

              if (is.na(x)) {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Missing a delay distribution parameter"
                  )
              }

              if ((!is.na(x)) & x <= 0) {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Delay parameter is not greater than 0"
                  )
              }
            } else {
              x <- delay_param[i, j]
              x <- strsplit(x, ";")[[1]]

              if ("NA" %in% x) {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Missing a delay distribution parameter"
                  )
              }
              if ((!("NA" %in% x)) & any(x <= 0) &
                delay_dist[i, j] == "unif") {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Delay distribution parameters are not greater than 0"
                  )
              }

              if ((!("NA" %in% x)) &
                any(x <= 0) & delay_dist[i, j] == "gamma") {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Delay distribution parameters are not greater than 0"
                  )
              }

              if ((!("NA" %in% x)) &
                any(x <= 0) & delay_dist[i, j] == "weibull") {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "Delay distribution parameters are not greater than 0"
                  )
              }

              if ((!("NA" %in% x)) & x[2] < 0 &
                delay_dist[i, j] == "lnorm") {
                issues <-
                  c(
                    issues,
                    paste0("Network Input"),
                    node_names[i],
                    "lnorm service parameter (sdlog) is less than 0"
                  )
              }
            }
          }
        }
      }


      ### Testing if there is at least 1 row of capacity and ext_arrival rate for each service point###

      row_test <- as.data.frame(cal[, 1:2])

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- row_test[which(row_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]


          if (nrow(x) == 0) {
            issues <-
              c(
                issues,
                "Calendar",
                node_names[i],
                paste0("Missing ", j, " input rows")
              )
          }
        }
      }

      ### Testing that every line in the caledar template has a value entry###

      value_test <- as.data.frame(cal)

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), 5]

          if (length(x) > 0) {
            if (any(is.na(x))) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Missing ", j, " value entry in calendar")
                )
            }
            if (!any(is.na(x))) {
              if (any(x < 0)) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0("Negative ", j, " value entry in calendar")
                  )
              }
            }
            if (!any(is.na(x))) {
              if (j == "cap" & all(x == 0)) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0("All zero ", j, " values entered in calendar")
                  )
              }
            }
          }
        }
      }

      ### Testing that nodes that have 2+ lines in the calendar have any values in the start and end columns ###
      value_test <- as.data.frame(cal)

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]


          if (nrow(x) > 1) {
            start <- x[, 3]
            end <- x[, 4]

            if (any(is.na(start))) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Missing start value(s) in ", j, " calendar")
                )
            }

            if (any(is.na(end))) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Missing end value(s) in ", j, " calendar")
                )
            }
          }
        }
      }


      ### Testing that nodes that have a zero in the first start line in the calendar ###

      value_test <- as.data.frame(cal)


      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]

          if (nrow(x) != 0) {
            if (!is.na(x[1, 3])) {
              start <- x[1, 3]
              if (start != 0) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0("Non-Zero Initial Start Time in ", j, " calendar")
                  )
              }
            }
            if (is.na(x[1, 3])) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0("Non-Zero Initial Start Time in ", j, " calendar")
                )
            }
          }
        }
      }



      ### Testing that nodes that have 2+ lines in the calendar have matching values in the start and end columns ###

      value_test <- as.data.frame(cal)

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]


          if (nrow(x) > 1) {
            start <- x[, 3]
            end <- x[, 4]

            start_tail <- tail(start, -1)
            end_head <- head(end, -1)

            start_tail[is.na(start_tail)] <- 0
            end_head[is.na(end_head)] <- 0



            if (any(!(start_tail == end_head))) {
              issues <-
                c(
                  issues,
                  "Calendar",
                  node_names[i],
                  paste0(
                    "Start & End values don't match up sequentially in ",
                    j,
                    " calendar"
                  )
                )
            }
          }
        }
      }

      ### Testing that nodes that have ascending start and end values ###

      value_test <- as.data.frame(cal)

      for (j in c("cap", "ext_arr")) {
        for (i in 1:node_number) {
          x <- value_test[which(value_test[, 1] == j), ]
          x <- x[which(x[, 2] == node_names[i]), ]


          if (nrow(x) > 1) {
            start <- x[, 3]
            end <- x[, 4]

            if (!any(is.na(start))) {
              if (any(diff(start) <= 0)) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0(
                      "Start values don't increase sequentially in ",
                      j,
                      " calendar"
                    )
                  )
              }
            }
            if (!any(is.na(end))) {
              if (any(diff(end) <= 0)) {
                issues <-
                  c(
                    issues,
                    "Calendar",
                    node_names[i],
                    paste0(
                      "End values don't increase sequentially in ",
                      j,
                      " calendar"
                    )
                  )
              }
            }
          }
        }
      }


      ### Testing that there are arrivals to at least one node ###

      value_test <- as.data.frame(cal)

      x <- value_test[which(value_test[, 1] == "ext_arr"), 5]
      if (!any(is.na(x))) {
        if (all(x == 0)) {
          issues <-
            c(
              issues,
              "Calendar",
              "All",
              paste0(
                "No Arrival rates to any service point in the ext_arr calendar"
              )
            )
        }
      }




      ####



      output$issues <- renderTable(
        {
          if (length(issues) == 0) {
            issues <- c("Complete", "Complete", "Complete")
          }
          issues <- matrix(
            data = issues,
            ncol = 3,
            byrow = T
          )

          colnames(issues) <- c("Location", "Service Point", "Issue")

          issues
        },
        striped = T,
        bordered = T,
        align = "c",
        caption = '<font size=4 color="red"><strong><p>Issues Log</p></strong></font>',
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL)
      )



      output$means <- renderTable(
        {
          if (length(issues) == 0) {
            var <- var()
            cal <- cal()

            mean_table <- c()

            x <- input$service_points
            x <- unique(x)
            rownames(x) <- 1:nrow(x)
            x <- trimws(x = x, which = "both")
            x <- gsub(x = x, pattern = " ", "_")
            sp <- x[which(x != "")]

            node_number <- length(sp)

            node_names <- sp


            for (i in 1:node_number) {
              pars <-
                as.numeric(unlist(strsplit(
                  as.character(var$serv_dist_param[i]), ";"
                )))

              if (var$serv_dist[i] == "exp") {
                mean_table <- c(mean_table, 1 / pars)
              } else if (var$serv_dist[i] == "unif") {
                mean_table <- c(mean_table, (pars[1] + pars[2]) / 2)
              } else if (var$serv_dist[i] == "lnorm") {
                mean_table <- c(mean_table, exp(pars[1] + 0.5 * (pars[2])^2))
              } else if (var$serv_dist[i] == "weibull") {
                mean_table <- c(mean_table, pars[2] * (gamma(1 + 1 / pars[1])))
              } else if (var$serv_dist[i] == "gamma") {
                mean_table <- c(mean_table, pars[1] / pars[2])
              } else {
                mean_table <- c(mean_table, c("Error in Mean Calculation"))
              }
            }

            mean_table <- as.data.frame(mean_table)
            rownames(mean_table) <- node_names
            colnames(mean_table) <- "Mean Length of Service"
            mean_table
          }
        },
        striped = T,
        bordered = T,
        align = "c",
        rownames = T,
        caption = "LoS Means",
        caption.placement = getOption("xtable.caption.placement", "top"),
        caption.width = getOption("xtable.caption.width", NULL)
      )




      output$download_buttons <- renderUI({
        if (length(issues) == 0) {
          fluidRow(
            column(
              6,
              align = "center",
              downloadButton(
                outputId = session$ns("var_dl"),
                label = "Network Template Download",
                style = "padding:16px; font-size:110%"
              )
            ),
            column(
              6,
              align = "center",
              downloadButton(
                outputId = session$ns("cal_dl"),
                label = "Calendar Download",
                style = "padding:16px; font-size:110%"
              )
            )
          )
        }
      })

      output$j2st <- renderUI({
        if (length(issues) == 0) {
          column(6, align = "center", actionButton(inputId = session$ns("j2PSR"), label = c(
            tagList("Move to Simulation Tool", icon("arrow-right"))
          )))
        }
      })
    })




    #### Creates the wizard template downloader####

    ### Creates the Var_input downloader###
    output$var_dl <- downloadHandler(
      filename = "var_input.csv",
      content = function(file) {
        write.csv(var(), file, row.names = TRUE)
      }
    )
    ### Creates the cal_input downloader###
    output$cal_dl <- downloadHandler(
      filename = "cal_input.csv",
      content = function(file) {
        write.csv(cal(), file, row.names = FALSE)
      }
    )

    list(var = var, cal = cal)
  })
}
