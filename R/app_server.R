#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #### SHINY SERVER CODE (INC SIM CODE) ####

  #### Figures for Modals####
  logger::log_debug("Setting up figures for modals.")
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



  observeEvent(input$jb2i, {
    updateTabsetPanel(session, "navbar",
      selected = "Introduction"
    )
  })


  observeEvent(input$j2PSR2, {
    updateTabsetPanel(session, "navbar",
      selected = "2. Simulation Setup & Run"
    )
  })


  observeEvent(input$j2s, {
    updateTabsetPanel(session, "navbar",
      selected = "W1. Setup"
    )
  })

  observeEvent(input$j2de, {
    showTab(inputId = "navbar", target = "W2. Data Entry")
    showTab(inputId = "navbar", target = "Service Distribution Tool")
    updateTabsetPanel(session, "navbar",
      selected = "W2. Data Entry"
    )
  })

  observeEvent(input$j2ftd, {
    showTab(inputId = "navbar", target = "W3. Final Wizard Tables & Download")
    updateTabsetPanel(session, "navbar",
      selected = "W3. Final Wizard Tables & Download"
    )
  })

  observeEvent(input$jb2de, {
    showTab(inputId = "navbar", target = "W2. Data Entry")
    updateTabsetPanel(session, "navbar",
      selected = "W2. Data Entry"
    )
  })

  observeEvent(input$j2PSR, {
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

  observeEvent(input$jb2i2, {
    updateTabsetPanel(session, "navbar",
      selected = "Introduction"
    )
  })

  observeEvent(input$jb2niv, {
    updateTabsetPanel(session, "navbar",
      selected = "1. Network Import & Visualisation"
    )
  })



  ##### START OF DYNAMIC WIZARD SERVER CODE ######
  logger::log_debug("Starting wizard.")

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
                inputId = paste0("transition_", j, "_", i),
                label = paste("Proportion from", all_names[j], "to", all_names[i]),
                value = 0,
                min = 0,
                max = 1,
                step = 0.001
              ),
              selectInput(
                inputId = paste0("delay_dist_", j, "_", i),
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
                    "input.",
                    paste0("delay_dist_", j, "_", i),
                    " == 'None'"
                  ),
                  shinyjs::disabled(column(
                    12,
                    textInput(
                      inputId = paste0("delay_param_none_1_", i),
                      value = "NA",
                      label = "No Parameters Required"
                    )
                  ))
                ),
                conditionalPanel(
                  condition = paste0(
                    "input.",
                    paste0("delay_dist_", j, "_", i),
                    " == 'Exponential'"
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = paste0("delay_param_exp_1_", j, "_", i),
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
                    "input.",
                    paste0("delay_dist_", j, "_", i),
                    " == 'log-Normal'"
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = paste0("delay_param_lnorm_1_", j, "_", i),
                      label = "meanlog",
                      value = "",
                      step = 0.0001
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = paste0("delay_param_lnorm_2_", j, "_", i),
                      label = "sdlog",
                      value = "",
                      step = 0.0001
                    )
                  )
                ),
                conditionalPanel(
                  condition = paste0(
                    "input.",
                    paste0("delay_dist_", j, "_", i),
                    " == 'Uniform'"
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = paste0("delay_param_unif_1_", j, "_", i),
                      label = "Min",
                      value = "",
                      min = 0,
                      step = 0.0001
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = paste0("delay_param_unif_2_", j, "_", i),
                      label = "Max",
                      value = "",
                      min = 0,
                      step = 0.0001
                    )
                  )
                ),
                conditionalPanel(
                  condition = paste0(
                    "input.",
                    paste0("delay_dist_", j, "_", i),
                    " == 'Weibull'"
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = paste0("delay_param_weibull_1_", j, "_", i),
                      label = "Shape",
                      value = "",
                      step = 0.0001
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = paste0("delay_param_weibull_2_", j, "_", i),
                      label = "Scale",
                      value = "",
                      step = 0.0001
                    )
                  )
                ),
                conditionalPanel(
                  condition = paste0(
                    "input.",
                    paste0("delay_dist_", j, "_", i),
                    " == 'Gamma'"
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = paste0("delay_param_gamma_1_", j, "_", i),
                      label = "Shape",
                      value = "",
                      step = 0.001
                    )
                  ),
                  column(
                    6,
                    numericInput(
                      inputId = paste0("delay_param_gamma_2_", j, "_", i),
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
                  inputId = paste0("transition_", j, "_", i),
                  label = paste("Proportion from", all_names[j], "to", all_names[i]),
                  value = 0
                )
              ),
              shinyjs::disabled(
                textInput(
                  inputId = paste0("delay_dist_", j, "_", i),
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
                  inputId = paste0("delay_param_none_1_", i),
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
              inputId = paste0("serv_dist_", i),
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
            condition = paste0("input.", paste0("serv_dist_", i), " == 'Exponential'"),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_exp_1_", i),
                label = "Rate",
                value = "",
                step = 0.001,
                min = 0
              )
            )
          ),
          conditionalPanel(
            condition = paste0("input.", paste0("serv_dist_", i), " == 'log-Normal'"),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_lnorm_1_", i),
                label = "meanlog",
                value = "",
                step = 0.001
              )
            ),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_lnorm_2_", i),
                label = "sdlog",
                value = "",
                step = 0.001
              )
            )
          ),
          conditionalPanel(
            condition = paste0("input.", paste0("serv_dist_", i), " == 'Uniform'"),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_unif_1_", i),
                label = "Min",
                value = "",
                step = 0.001,
                min = 0
              )
            ),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_unif_2_", i),
                label = "Max",
                value = "",
                step = 0.001,
                min = 0
              )
            )
          ),
          conditionalPanel(
            condition = paste0("input.", paste0("serv_dist_", i), " == 'Weibull'"),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_weibull_1_", i),
                label = "Shape",
                value = "",
                step = 0.001
              )
            ),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_weibull_2_", i),
                label = "Scale",
                value = "",
                step = 0.001
              )
            )
          ),
          conditionalPanel(
            condition = paste0("input.", paste0("serv_dist_", i), " == 'Gamma'"),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_gamma_1_", i),
                label = "Shape",
                value = "",
                step = 0.001
              )
            ),
            column(
              2,
              numericInput(
                inputId = paste0("serv_param_gamma_2_", i),
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
              inputId = paste0("ext_q_", i),
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
              inputId = paste0("int_q_", i),
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
                inputId = paste0("ext_arr_", i),
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
                inputId = paste0("cap_", i),
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
              outputId = "var_dl",
              label = "Network Template Download",
              style = "padding:16px; font-size:110%"
            )
          ),
          column(
            6,
            align = "center",
            downloadButton(
              outputId = "cal_dl",
              label = "Calendar Download",
              style = "padding:16px; font-size:110%"
            )
          )
        )
      }
    })

    output$j2st <- renderUI({
      if (length(issues) == 0) {
        column(6, align = "center", actionButton(inputId = "j2PSR", label = c(
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


  #### Service Distribution Tool Module ####
  mod_distribution_tool_server("distribution_tool")

  logger::log_debug("Wizard complete.")
  ###### END OF WIZARD#######
  ###### START OF SIMULATION TOOL##########

  #### Template upload and checks ####
  output$contents1 <- renderTable(
    {
      if (input$disp1 == TRUE) {
        if (input$w_temp == 0) {
          req(input$file1)
          df <- read.csv(input$file1$datapath,
            header = TRUE,
            sep = ","
          )
          rownames(df) <- df[, 1]
          df <- df[, -1]
          colnames(df)[1:(which(colnames(df) == "serv_dist") - 1)] <-
            rownames(df)
          df
        } else {
          var()
        }
      }
    },
    rownames = TRUE,
    caption = "Variable Inputs",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )


  output$contents2 <- renderTable(
    {
      if (input$disp2 == TRUE) {
        if (input$w_temp == 0) {
          req(input$file2)
          df <- read.csv(input$file2$datapath,
            header = TRUE,
            sep = ","
          )
          df
        } else {
          cal()
        }
      }
    },
    caption = "Calendar Inputs",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )




  issues <- eventReactive(input$go_viz, {
    req(input$file1)
    req(input$file2)

    df <- read.csv(input$file1$datapath,
      header = TRUE,
      sep = ","
    )

    rownames(df) <- df[, 1]
    df <- df[, -1]
    colnames(df)[1:(which(colnames(df) == "serv_dist") - 1)] <-
      rownames(df)


    df2 <- read.csv(input$file2$datapath,
      header = TRUE,
      sep = ","
    )

    issues <- c()

    var <- df
    cal <- df2

    x <- rownames(var[which(!is.na(var[, 1])), ])
    x <- unique(x)
    # rownames(x)<-1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    sp <- x[which(x != "")]

    x <- rownames(var[which(is.na(var[, 1])), ])
    x <- unique(x)
    # rownames(x)<-1:nrow(x)
    x <- trimws(x = x, which = "both")
    x <- gsub(x = x, pattern = " ", "_")
    exit <- x[which(x != "")]

    node_number <- length(sp)
    exit_number <- length(exit)

    node_names <- sp

    exit_names <- exit

    all_names <- c(node_names, exit_names)




    ### Testing if the names match between templates###

    cal_names <- unique(cal$node)


    if (length(node_names) != length(cal_names) |
      any(!(node_names %in% cal_names))) {
      issues <-
        c(
          issues,
          c(
            paste0("Network & Cal input"),
            "All",
            "Service point names do not match between templates"
          )
        )
    }





    ### Testing if the transition matrix has rowsums of 1###

    f <- var[1:node_number, 1:length(all_names)]
    indx <- sapply(f, is.factor)
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
            "Transition matrix contains value outside required range (replace with value between 0 and 1)",
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
            paste("Row sum does not equal 1 (Currently:", x, ")", sep = "")
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
      x <- serv_dist_param[i, 1]

      if (is.na(x)) {
        issues <-
          c(
            issues,
            paste0("Network Input"),
            node_names[i],
            "Missing a service distribution"
          )
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

        # if((!("NA" %in% x))&x[2]<0&serv_dist_param[i,1]=="lnorm"){
        #
        #   issues<-c(issues,paste0("Network Input"),node_names[i],"lnorm service parameter (sdlog) is less than 0")
        #
        # }
      }
    }


    #### Testing if the Queue inputs are correct #
    iq <- var$int_queue


    eq <- var$ext_queue

    for (i in 1:node_number) {
      x <- iq[i]

      if (x == Inf) {
        x <- 9999
      }

      if (is.na(x)) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter Internal Queue Value")
          ))
      }


      if (as.numeric(x) %% 1 != 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter an integer Internal Queue Value")
          ))
      }

      if (as.numeric(x) < 0) {
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

      if (x == Inf) {
        x <- 9999
      }

      if (is.na(x)) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter External Queue Value")
          ))
      }

      if (as.numeric(x) %% 1 != 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter an integer External Queue Value")
          ))
      }

      if (as.numeric(x) < 0) {
        issues <-
          c(issues, c(
            paste0("Network Input"),
            node_names[i],
            paste("Need to enter a positive External Queue Value")
          ))
      }
    }




    ### Testing if the delay distribution inputs are correct ###

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
        if (!is.na(delay_param[i, j])) {
          x <- delay_dist[i, j]

          if (is.na(x)) {
            issues <-
              c(
                issues,
                paste0("Network Input"),
                node_names[i],
                "Missing a delay distribution "
              )
          }
        }
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

            # if((!("NA" %in% x))&x[2]<0&delay_dist[i,j]=="lnorm"){
            #
            #   issues<-c(issues,paste0("Network Input"),node_names[i],"lnorm service parameter (sdlog) is less than 0")
            #
            # }
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

    ### Testing that every line in the calendar template has a value entry###

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
  })

  output$file_check_issues <- renderTable(
    {
      issues <- issues()
      issues
    },
    striped = T,
    bordered = T,
    align = "c",
    caption = "Issues Log",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL)
  )




  #### NETWORK VISUALISATION ####
  logger::log_debug("Creating network visualisation.")
  viz <- eventReactive(input$go_viz, {
    if (input$w_temp == 0) {
      req(input$file1)
      req(input$file2)

      var_input <-
        read.csv(input$file1$datapath,
          header = TRUE,
          sep = ","
        )
      rownames(var_input) <- var_input[, 1]
      var_input <- var_input[, -1]

      cal_input <-
        read.csv(input$file2$datapath,
          header = TRUE,
          sep = ","
        )

      issues <- issues()

      req(issues[1, 1] == "Complete")
    } else {
      var_input <- var()

      var_input <- as.data.frame(var_input)

      f <- var_input
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      var_input <- as.data.frame(f)

      f <- var_input[, 1:nrow(var_input)]
      indx <- 1:nrow(var_input)
      f[, indx] <- lapply(f[indx], function(x) {
        as.numeric(x)
      })
      var_input[, 1:nrow(var_input)] <- f



      var_input$ext_queue <- as.numeric(var_input$ext_queue)

      var_input$int_queue <- as.numeric(var_input$int_queue)



      cal_input <- cal()
      cal_input <- as.data.frame(cal_input)
      cal_input$metric <- as.character(cal_input$metric)
      cal_input$node <- as.character(cal_input$node)
      cal_input$start <- as.numeric(as.character(cal_input$start))
      cal_input$end <- as.numeric(as.character(cal_input$end))
      cal_input$value <- as.numeric(as.character(cal_input$value))
    }

    nodes <-
      rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
        "serv_dist") - 1], na.rm = T) != 0), ])
    exits <-
      rownames(var_input[which(rowSums(var_input[, 1:which(colnames(var_input) ==
        "serv_dist") - 1], na.rm = T) == 0), ])
    # ext_arr<-rownames(var_input[which(var_input$ext_arr>0),])

    ext_arr <-
      unique(cal_input$node[which(cal_input$metric == "ext_arr" &
        cal_input$value > 0)])


    delay_dist <-
      var_input[, (nrow(var_input) + 5):(nrow(var_input) + nrow(var_input) + 4)] ## Import the template in csv
    rownames(delay_dist) <- rownames(var_input)
    colnames(delay_dist) <- rownames(var_input)
    delay_dist[which(delay_dist == "", arr.ind = T)] <- NA

    delay_param <-
      var_input[, (nrow(var_input) + nrow(var_input) + 5):(ncol(var_input))] ## Import the template in csv
    rownames(delay_param) <- rownames(var_input)
    colnames(delay_param)[1:nrow(delay_param)] <- rownames(var_input)
    delay_param[which(delay_param == "", arr.ind = T)] <- NA


    from <- c(0)
    to <- c(0)


    for (i in 1:nrow(delay_dist)) {
      for (j in 1:nrow(delay_dist)) {
        if (!is.na(delay_dist[i, j])) {
          from <- c(from, i)
          to <- c(to, j)
        }
      }
    }

    delay_list <- cbind(from, to)


    tmp <- rownames(delay_dist)
    delay_exits <-
      tmp[c(delay_list[, 2])][!tmp[c(delay_list[, 2])] %in% nodes]


    var_input$serv_dist[which(rownames(var_input) %in% exits[!(exits %in% delay_exits)])] <-
      NA
    var_input$serv_dist_param[which(rownames(var_input) %in% exits[!(exits %in% delay_exits)])] <-
      NA

    cap_min <- vector()
    for (i in nodes) {
      cap_min <-
        c(cap_min, min(cal_input$value[which(cal_input$node == i &
          cal_input$metric == "cap")]))
    }


    cap_max <- vector()
    for (i in nodes) {
      cap_max <-
        c(cap_max, max(cal_input$value[which(cal_input$node == i &
          cal_input$metric == "cap")]))
    }


    cal_tooltip <- vector()

    for (i in nodes) {
      tmp <- cal_input[which(cal_input$node == i &
        cal_input$metric == "cap"), ]
      tmp2 <- vector()

      for (j in 1:nrow(tmp)) {
        tmp3 <-
          paste(
            "\n",
            "Start:",
            tmp[j, 3],
            "End:",
            tmp[j, 4],
            "Capacity:",
            tmp[j, 5],
            "//"
          )


        tmp2 <- c(tmp2, tmp3)
      }
      tmp2 <- paste(tmp2, collapse = "")
      tmp2 <- paste("Capacity Calendar //", tmp2)
      cal_tooltip <- c(cal_tooltip, tmp2)
    }



    # Create a node data frame (ndf)

    ndf1 <- DiagrammeR::create_node_df(
      n = length(nodes),
      type = "lower",
      label = c(nodes),
      fillcolor = "deepskyblue1",
      color = "black",
      fontcolor = "black",
      shape = "square",
      tooltip = cal_tooltip,
      fixedsize = FALSE
    )



    ndf2 <- DiagrammeR::create_node_df(
      n = length(exits),
      type = "lower",
      label = c(exits),
      fillcolor = "green",
      color = "black",
      fontcolor = "black",
      shape = "diamond",
      tooltip = "Exit",
      fixedsize = FALSE
    )




    ndf3 <- DiagrammeR::create_node_df(
      n = length(ext_arr),
      type = "lower",
      label = as.numeric(c(length(c(
        nodes, exits
      )) + 1):(length(c(
        nodes, exits
      )) + length(ext_arr))),
      fillcolor = "white",
      fontcolor = "white",
      shape = "square",
      color = "white"
    )

    ndf <- DiagrammeR::combine_ndfs(ndf1, ndf2, ndf3)

    # Create an edge data frame (edf)
    f <- vector()
    t <- vector()
    l <- vector()
    edge_col <- vector()
    edge_tip <- vector()

    for (i in 1:length(nodes)) {
      for (j in 1:length(c(nodes, exits))) {
        if (var_input[i, j] > 0) {
          f <- c(f, i)
          t <- c(t, j)

          if (!is.na(delay_dist[i, j])) {
            l <- c(l, paste0(round(var_input[i, j] * 100, digits = 2), "%"))
            edge_col <- c(edge_col, "sienna2")

            if (delay_dist[i, j] == "exp") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- 1 / pars[1]
            } else if (delay_dist[i, j] == "unif") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- (pars[1] + pars[2]) / 2
            } else if (delay_dist[i, j] == "lnorm") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- exp(pars[1] + 0.5 * (pars[2])^2)
            } else if (delay_dist[i, j] == "weibull") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- pars[2] * (gamma(1 + (1 / pars[1])))
            } else if (delay_dist[i, j] == "gamma") {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              delay_mean <- pars[1] / pars[2]
            } else {
              pars <-
                as.numeric(unlist(strsplit(
                  x = as.character(delay_param[i, j]), split = ";"
                )))
              tmp2 <-
                do.call(get(paste0("r", delay_dist[i, j])), as.list(c(10^7, pars))) # Creates a service time
              delay_mean <- mean(tmp2)
            }

            edge_tip <-
              c(
                edge_tip,
                paste0(
                  "Mean Delay: ",
                  delay_mean,
                  " (Delay Dist: ",
                  delay_dist[i, j],
                  ")"
                )
              )
          } else {
            l <- c(l, paste0(round(var_input[i, j] * 100, digits = 2), "%"))
            edge_col <- c(edge_col, "black")
            edge_tip <- c(edge_tip, paste0("No Delay"))
          }
        }
      }
    }


    edf1 <- DiagrammeR::create_edge_df(
      from = f,
      to = t,
      # rel = c("leading_to"),
      label = l,
      color = edge_col,
      fontcolor = edge_col,
      tooltip = edge_tip
    )


    edf2 <-
      DiagrammeR::create_edge_df(
        from = c(length(c(nodes, exits)) + 1):(length(c(nodes, exits)) + length(ext_arr)),
        to = as.numeric(which(rownames(var_input) %in% ext_arr)),
        # rel = c("leading_to"),
        label = as.character("Arrivals"),
        color = "red",
        fontcolor = "red",
        tooltip = "Arrival"
      )

    edf <- DiagrammeR::combine_edfs(edf1, edf2)




    # Create a list of average LOS
    LOS <- vector()


    for (i in nodes) {
      arr.dist <- var_input$serv_dist[which(rownames(var_input) == i)]
      pars <-
        as.numeric(unlist(strsplit(
          as.character(var_input$serv_dist_param[which(rownames(var_input) == i)]), ";"
        )))


      if (arr.dist == "exp") {
        tmp3 <- 1 / pars
        LOS <- c(LOS, tmp3)
      } else if (arr.dist == "unif") {
        tmp3 <- (pars[1] + pars[2]) / 2
        LOS <- c(LOS, tmp3)
      } else if (arr.dist == "lnorm") {
        tmp3 <- exp(pars[1] + 0.5 * (pars[2])^2)
        LOS <- c(LOS, tmp3)
      } else if (arr.dist == "weibull") {
        tmp3 <- pars[2] * (gamma(1 + (1 / pars[1])))
        LOS <- c(LOS, tmp3)
      } else if (arr.dist == "gamma") {
        tmp3 <- pars[1] / pars[2]
        LOS <- c(LOS, tmp3)
      } else {
        tmp2 <-
          do.call(get(paste0("r", arr.dist)), as.list(c(10^7, pars))) # Creates a service time
        tmp3 <- mean(tmp2)

        LOS <- c(LOS, tmp3)
      }
    }
    LOS <- round(LOS, digits = 2)


    TAC <- vector()

    for (i in nodes) {
      tmp <- cal_input[which(cal_input$node == i &
        cal_input$metric == "cap"), ]

      if (nrow(tmp) == 1) {
        TAC <- c(TAC, tmp$value)
      }
      if (nrow(tmp) > 1) {
        tmp2 <- sum(tmp$value * (tmp$end - tmp$start)) / max(tmp$end)
        TAC <- c(TAC, tmp2)
      }
    }

    TAC <- ceiling(TAC)


    node_labels <- vector()

    for (i in 1:length(nodes)) {
      tmp1 <-
        paste0(
          nodes[i],
          "\n",
          " LOS: ",
          LOS[i],
          "\n",
          "Av Cap: ",
          TAC[i],
          "\n",
          "IQC: ",
          var_input$int_queue[i],
          "\n",
          "EQC: ",
          var_input$ext_queue[i]
        )

      node_labels <- c(node_labels, tmp1)
    }

    if (input$disp3 == TRUE) {
      ndf$label[1:length(nodes)] <- node_labels
    }


    # Create a graph with the ndf and edf
    graph <-
      DiagrammeR::create_graph(
        nodes_df = ndf,
        edges_df = edf
      )

    graph$global_attrs[1, "value"] <- "dot"
    graph$global_attrs[4, "value"] <- 20
    graph$global_attrs[6, "value"] <- "false"
    graph$global_attrs[14, "value"] <- 20
    graph$global_attrs[17, "value"] <- 1

    graph$global_attrs <-
      rbind(graph$global_attrs, c("rankdir", "LR", "graph"))
    graph$global_attrs <-
      rbind(graph$global_attrs, c("splines", "true", "graph"))

    showTab(inputId = "navbar", target = "2. Simulation Setup & Run")

    output$next_button <- renderUI({
      column(6, align = "center", actionButton(inputId = "j2PSR2", label = c(tagList(
        "Next", icon("arrow-right")
      ))))
    })





    DiagrammeR::render_graph(graph)
  })

  output$network <- DiagrammeR::renderGrViz({
    viz()
  })

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





    if (input$w_temp == 0) {
      req(input$file1)

      df <- read.csv(input$file1$datapath,
        header = TRUE,
        sep = ","
      )
      rownames(df) <- df[, 1]
      df <- df[, -1]
      colnames(df)[1:(which(colnames(df) == "serv_dist") - 1)] <-
        rownames(df)
    } else {
      var_input <- var()

      var_input <- as.data.frame(var_input)

      f <- var_input
      indx <- sapply(f, is.factor)
      f[indx] <- lapply(f[indx], function(x) {
        as.character(x)
      })
      var_input <- as.data.frame(f)

      f <- var_input[, 1:nrow(var_input)]
      indx <- 1:nrow(var_input)
      f[, indx] <- lapply(f[indx], function(x) {
        as.numeric(x)
      })
      var_input[, 1:nrow(var_input)] <- f



      var_input$ext_queue <- as.numeric(var_input$ext_queue)

      var_input$int_queue <- as.numeric(var_input$int_queue)

      df <- var_input
    }
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
        if (input$w_temp == 0) {
          req(input$file1)
          req(input$file2)

          var_input <-
            read.csv(input$file1$datapath,
              header = TRUE,
              sep = ","
            )

          syst_names <-
            cbind(as.numeric(c(1:nrow(var_input))), as.character(var_input[, 1]))
          syst_names_single <- syst_names[, 2]

          var_input <- var_input[, -1]
          rownames(var_input) <- 1:nrow(var_input)
          colnames(var_input)[1:nrow(var_input)] <- c(1:nrow(var_input))

          cal_input <-
            read.csv(input$file2$datapath,
              header = TRUE,
              sep = ","
            )
          cal_input$node <- as.character(cal_input$node)
        } else {
          var_input <- var()

          var_input <- as.data.frame(var_input)

          f <- var_input
          indx <- sapply(f, is.factor)
          f[indx] <- lapply(f[indx], function(x) {
            as.character(x)
          })
          var_input <- as.data.frame(f)

          f <- var_input[, 1:nrow(var_input)]
          indx <- 1:nrow(var_input)
          f[, indx] <- lapply(f[indx], function(x) {
            as.numeric(x)
          })
          var_input[, 1:nrow(var_input)] <- f

          var_input$ext_queue <- as.numeric(var_input$ext_queue)

          var_input$int_queue <- as.numeric(var_input$int_queue)

          syst_names <-
            cbind(as.numeric(c(1:nrow(var_input))), as.character(rownames(var_input)))
          syst_names_single <- syst_names[, 2]

          rownames(var_input) <- 1:nrow(var_input)
          colnames(var_input)[1:nrow(var_input)] <- c(1:nrow(var_input))

          cal_input <- cal()
          cal_input <- as.data.frame(cal_input)
          cal_input$metric <- as.character(cal_input$metric)
          cal_input$node <- as.character(cal_input$node)
          cal_input$start <- as.numeric(as.character(cal_input$start))
          cal_input$end <- as.numeric(as.character(cal_input$end))
          cal_input$value <- as.numeric(as.character(cal_input$value))
        }

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
          time_unit = input$time_unit,
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
