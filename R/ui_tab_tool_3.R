ui_tab_tool_3 <- function() {
  tabPanel(
    "3. Simulation Outputs",
    navlistPanel(
      id = "Simulation Outputs",
      tabPanel(
        title = "Output Interpretation",
        icon = icon("book"),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(
            9,
            h2("Output Interpretation"),
            p(
              "The pages on the left show the key results from the simulation, a description of which can be found below.
                        Depending on the number of replications run, the graphs and tables may take a moment to render (Click through the tabs to ensure the rendering begins).
                        Return to the previous pages using the navigation bar above."
            ),
            hr(),
            h4(
              strong(
                "Warm-Up Period Assistance (Only available in Trial Simulation Mode)"
              )
            ),
            p(
              "The warm-up period represents the time it takes for a simulation to reach stable running conditions and after which results can be recorded.
                         As each simulation starts from empty, it is important that the warm-up period be long enough so that the results collected are reflective of
                         the modelled system and not an emptier version. To determine a suitable warm-up period, the Warm-Up Period Assistance tab shows the total number of people in the
                                            system through time. This metric can be used to work out how long the warm-up period needs to be."
            ),
            hr(),
            h4(
              strong(
                "Average Through Time Plot (Available in both Trial and Full Simulation Modes)"
              )
            ),
            p(
              "A summary plot showing how each of the 5 time varying parameters vary through time with mean values plotted with envelopes of 50%, 95% and 99% percentile data.
                        This should allow the overarching trends in these metrics to be understood in a single figure and also how changes and shift in one metrics influence changes in
                        others. Variation in the mean lines could be the result of two different factors: 1) Sample size being too small (increase # of replications) or 2) The system has
                        inherent variation, either driven by random dynamics or the prescribed calendar schedule. This plot allows the user to quickly understand the dynamics of the system
                        e.g. if a unit has reached capcaity and stayed full, if the queue length has stabilised or if it is continuously increasing, whether the number of patients being delayed
                        due to capacity is lower than expected values."
            ),
            hr(),
            h4(
              strong(
                "Service Point & Pathway Statistics (Only available in Full Simulation Mode)"
              )
            ),
            p(
              "These pages contains multiple tables looking at 7 key metrics of the patient experience (broken down by service node or summarised over the pathway):"
            ),
            tags$ol(
              tags$li(
                strong("Total time in system"),
                "- Amount of time between arriving at the first node and leaving the last on a patient pathway."
              ),
              tags$li(
                strong("Wait"),
                "- Amount of time between arriving at a queue and starting service."
              ),
              tags$li(
                strong("Time Delayed (Capacity Driven)"),
                "- Amount of time experiencing a capacity driven delay."
              ),
              tags$li(
                strong("Time Delayed (Transition)"),
                "- Amount of time experiencing a transition delay."
              ),
              tags$li(
                strong("Length of Stay"),
                "- Amount of time between starting service and departing to the next service."
              ),
              tags$li(
                strong("Delay to Transfer"),
                "- Amount of time between finishing service and departure (i.e. end of any delays)."
              ),
              tags$li(
                strong("Rejection Rate"),
                "- Number of external arrivals that were rejected due to full queues per time unit."
              )
            ),
            h4(
              strong(
                "Metrics Through Time Summaries (Only available in Full Simulation Mode)"
              )
            ),
            p(
              "5 values are monitored throughout the simulation so their changes through time can be investigated through time:"
            ),
            strong(tags$ul(
              tags$li("Patient Occupancy"),
              tags$li("Bed Occupancy"),
              tags$li("Capacity Driven Delay"),
              tags$li("Transition Delay"),
              tags$li("Queue")
            )),
            p("Each page on the left contains the same 5 tables/graphs:"),
            tags$ul(
              tags$li(
                strong("Top Left - Percentage time at level plot"),
                " - A graph showing the amount of time each Service Point spent at level of the metric
                                (e.g. Amount of time Service Point A had an occupancy of 5 patients). The distribution of bars informs how the metric has varied throughout the simulation,
                                for example if the bars appear reasonably symetric around a value then the system is showing signs of stability. On the other hand, if one bar dominates then
                                the system is showing signs of underlying system dynamics e.g. constantly at full capacity or following a strict calendar."
              ),
              br(),
              tags$li(
                strong("Top Right - Metric through time plot"),
                " - A graph showing the metric in question through time, split by Service Point and replicate (max 5 random replicatations).
                                These represent actual simulation runs that are then combined (across all replications) to form the summary outputs. These should not be used to infer specific results, but
                                are intended to be illustrative of the variation found within simulation."
              ),
              br(),
              tags$li(
                strong("Bottom Left - Percentiles summary table"),
                " - A table outlining the values associated with different percentile levels e.g. 90% of the time, the Service Point has an occupancy of 5 or less"
              ),
              br(),
              tags$li(
                strong("Bottom Right - Percentage time at level table"),
                " - Raw data used to construct 'Percentage time at level' plot.
                                Can be filtered and sorted by any column and also contains a cumulative sum which can be used to calcuate percentiles."
              ),
              br(),
              tags$li(
                strong("Bottom Centre - Average Over Simulation"),
                " - Average value for the metric in question when using the data from the entire simulation."
              )
            ),
            p(
              "All plots show a maximum of 5 replicates and have the same legends and colour coding for Service Points"
            ),
            hr()
          )
        )
      ),
      tabPanel(
        "Warm-Up Period Assistance",
        icon = icon("chart-line"),
        h2(strong("Warm-Up Period Assistance")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Trial Simulation'",
          p(
            "The warm-up period represents the time it takes for a simulation to reach stable running conditions and after which results can be recorded.
                         As each simulation starts from empty, it is important that the warm-up period be long enough so that the results collected are reflective of
                         the modelled system and not an emptier version. To determine a suitable warm-up period, find the time after which the total number of people in system has leveled out/ stabilised.
                                   For highly dynamic systems, you may also need to consult the average through time tab to see how the number of people in each service point and queue is changing. The
                                   warm-up period can be determined in the same way as before but needs to be the time required for all metrics to stabilise."
          ),
          plotOutput("tisp",
            height =
              "850px"
          )
        ),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          h2(strong(
            "Not Available in Full Simulation"
          ))
        )
      ),
      tabPanel(
        "Average Through Time Plot",
        icon = icon("chart-line"),
        h2(strong("Average Through Time Overview")),
        hr(),
        p(
          "The plot below shows how each of the 5 time varying parameters vary through time with mean values plotted with envelopes of 50%, 95% and 99% percentile data.
                        This should allow the overarching trends in these metrics to be understood in a single figure and also how changes and shift in one metrics influence changes in
                        others. Variation in the mean lines could be the result of two different factors: 1) Sample size being too small (increase # of replications) or 2) The system has
                        iherent variation, either driven by random dynamics or the prescribed calendar schedule. This plot allows the user to quickly understand the dynamics of the system
                        e.g. if a unit has reached capcaity and stayed full, if the queue length has stabilised or if it is continuously increasing, whether the number of patients being delayed
                        due to capacity is lower than expected values."
        ),
        plotOutput("multi_plot", height = "850px")
      ),
      tabPanel(
        "Service Point Statistics",
        icon = icon("table"),
        h2(strong("Service Point Statistics")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(column(
            12,
            align = "center", grVizOutput("tables_viz1", height = "400px")
          )),
          br(),
          fluidRow(
            column(
              4,
              align = "center",
              h4("Wait Time"),
              tableOutput("node_wait_summary"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Time Delayed (Capacity Driven)"),
              tableOutput("node_capacity_delay_summary"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Time Delayed (Transition)"),
              tableOutput("node_transition_delay_summary"),
              align = "center"
            )
          ),
          fluidRow(
            column(
              4,
              align = "center",
              h4("Length Of Stay"),
              tableOutput("node_loss"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Delay-To-Transfer"),
              tableOutput("node_dtts"),
              align = "center"
            ),
            column(
              width = 1,
              offset = 0,
              style = "padding:0px;"
            ),
            column(2, h4("Rejection Rate"), tableOutput("rejs"),
              align =
                "center"
            )
          )
        ),
        conditionalPanel(
          condition = "input.run_type=='Trial Simulation'",
          h2(
            strong("Not Available in Trial Simulation")
          )
        )
      ),
      tabPanel(
        "Pathway Statistics",
        icon = icon("table"),
        h2(strong("Pathway Statistics")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(column(
            12,
            align = "center", grVizOutput("tables_viz2", height = "400px")
          )),
          br(),
          fluidRow(
            column(
              4,
              align = "center",
              h4("Wait Time"),
              tableOutput("pat_wait_summary"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Time Delayed (Capacity Driven)"),
              tableOutput("pat_capacity_delay_summary"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Time Delayed (Transition)"),
              tableOutput("pat_transition_delay_summary"),
              align = "center"
            )
          ),
          fluidRow(
            column(
              4,
              align = "center",
              h4("Length Of Stay"),
              tableOutput("pat_loss"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Delay-To-Transfer"),
              tableOutput("pat_dtts"),
              align = "center"
            ),
            column(
              4,
              align = "center",
              h4("Total Time in System"),
              tableOutput("ttiss"),
              align = "center"
            )
          )
        ),
        conditionalPanel(
          condition = "input.run_type=='Trial Simulation'",
          h2(
            strong("Not Available in Trial Simulation")
          )
        )
      ),
      tabPanel(
        "Patient Occupancy Summary",
        icon = icon("user"),
        h2(strong("Patient Occupancy Summary")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Patient Occupancy level"),
              plotOutput("pto_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Patient Occupancy for 5 replicates"),
              plotOutput("o", height = "500px")
            )
          ),
          fluidRow(
            column(4, align = "center", dataTableOutput("opercentiles")),
            column(8, align = "center", dataTableOutput("pto_percent"))
          ),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = "padding:0px;"
            ),
            column(2, align = "center", dataTableOutput("avg_occupancy"))
          )
        ),
        conditionalPanel(
          condition = "input.run_type=='Trial Simulation'",
          h2(
            strong("Not Available in Trial Simulation")
          )
        )
      ),
      tabPanel(
        "Bed Occupancy Summary",
        icon = icon("bed"),
        h2(strong("Bed Occupancy Summary")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Bed Occupancy level"),
              plotOutput("ptb_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Bed Occupancy for 5 replicates"),
              plotOutput("b", height = "500px")
            )
          ),
          fluidRow(
            column(4, align = "center", dataTableOutput("bpercentiles")),
            column(8, align = "center", dataTableOutput("ptb_percent"))
          ),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = "padding:0px;"
            ),
            column(2, align = "center", dataTableOutput("avg_occ_bed"))
          )
        ),
        conditionalPanel(
          condition = "input.run_type=='Trial Simulation'",
          h2(
            strong("Not Available in Trial Simulation")
          )
        )
      ),
      tabPanel(
        "Capacity Driven Delay Summary",
        icon = icon("door-closed"),
        h2(strong("Capacity Driven Delay Summary")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Capacity Delay level"),
              plotOutput("ptd_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Capacity Delay Level for 5 replicates"),
              plotOutput("d", height = "500px")
            )
          ),
          fluidRow(
            column(4, align = "center", dataTableOutput("dpercentiles")),
            column(8, align = "center", dataTableOutput("ptd_percent"))
          ),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = "padding:0px;"
            ),
            column(2, align = "center", dataTableOutput("avg_delayed"))
          )
        ),
        conditionalPanel(
          condition = "input.run_type=='Trial Simulation'",
          h2(
            strong("Not Available in Trial Simulation")
          )
        )
      ),
      tabPanel(
        "Transition Delay Summary",
        icon = icon("expand-arrows-alt"),
        h2(strong("Transition Delay Summary")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Transition Delay level"),
              plotOutput("ptt_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Transition Delay Level for 5 replicates"),
              plotOutput("t", height = "500px")
            )
          ),
          fluidRow(
            column(4, align = "center", dataTableOutput("tpercentiles")),
            column(8, align = "center", dataTableOutput("ptt_percent"))
          ),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = "padding:0px;"
            ),
            column(2, align = "center", dataTableOutput("avg_transition"))
          )
        ),
        conditionalPanel(
          condition = "input.run_type=='Trial Simulation'",
          h2(
            strong("Not Available in Trial Simulation")
          )
        )
      ),
      tabPanel(
        "Queueing Summary",
        icon = icon("clock"),
        h2(strong("Queueing Summary")),
        hr(),
        conditionalPanel(
          condition = "input.run_type=='Full Simulation'",
          fluidRow(
            column(
              6,
              align = "center",
              h3("% time at Queue Length"),
              plotOutput("ptq_plot", height = "500px")
            ),
            column(
              6,
              align = "center",
              h3("Queue Length for 5 replicates"),
              plotOutput("q", height = "500px")
            )
          ),
          fluidRow(
            column(4, align = "center", dataTableOutput("qpercentiles")),
            column(8, align = "center", dataTableOutput("ptq_percent"))
          ),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              style = "padding:0px;"
            ),
            column(2, align = "center", dataTableOutput("avg_queue"))
          )
        ),
        conditionalPanel(
          condition = "input.run_type=='Trial Simulation'",
          h2(
            strong("Not Available in Trial Simulation")
          )
        )
      ),
      widths = c(3, 9),
      well = TRUE
    )
  )
}
