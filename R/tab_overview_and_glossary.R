tab_overview_and_glossary <- function() {
  tabPanel(
    "Overview & Glossary",
    navlistPanel(
      tabPanel(
        "Overview",
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(
            7,
            HTML(
              "
                      <h1>Overview</h1>
                      <p>PathSimR is a Discrete Event Simulation (DES) tool designed exclusively in R. Aimed at use within the healthcare sector, PathSimR uses relevant terminology and constructs healthcare focussed metrics as outputs. For more information about DES, please consult the technical documentation.</p>

                      <h1>Moving Away From Average-based Modelling</h1>
                      <p> Planning by averages involves a simple equation: Capacity= Arrivals x Length of Service.
                      So with an average of 10 arrivals per day and each patient staying an average of 5 days, the capacity should be 50 beds.
                      Real data shows however that most patients don't actually stay for 5 days exactly and instead show large amounts of variation in their length of stay.
                      Averaging data and therefore removing the variation component needs to be done with extreme caution.
                      Often planning service provision based on data that has been averaged will lead to underestimating the amount
                      of capacity that is required and could have serious impacts on waiting times and capacity driven delays.</p>


                      <h1>Key Features and Assumptions of PathSimR</h1>
                      <p>PathSimR is capable of simulating patient pathways consisting of a number of user defined service points. Each service point can take the form of any service/treatment location that has a prescribed capacity and service length. In the case of a bedded ward in a hospital, this would be the number of beds and treatment length whilst in the case of a GP clinic, this would be number of GPs on shift and appointment length.</p>
                      <p>Each service point along the pathway has a number of user defined parameters including:</p>
                      <ul>
                      <li>Calendar-dependent external arrival rates (assumed to be Poisson distributed)</li>
                      <li>Calendar-dependent capacities</li>
                      <li>Service times that can be modelled as any parameter distribution available in R</li>
                      <li>Internal and external queue capacities (including zero and infinite)</li>
                      </ul>
                      <p>Movement between service points and subsequently movement to an exit is managed through a transition rate matrix which describes the proportion of patient who move between two locations on the pathway.</p>
                      <p>PathSimR deals with both blocking after service (due to lack of downstream capacity in the network) and with user-specified delays/transition times between service points/service points and pathway exits. The former arises when there is no available capacity in an onward service point and no queue to join which forces a patient to reside in their current location, stopping a new patient from starting. The latter, transition delays, are user-defined and are implemented in matrices which describe pairwise delay distributions (and their parameters) between pathway locations.</p>
                      <p>The tool makes a handful of assumptions in order to simplify the modelled systems. Firstly, with respect to queues and service points, PathSimR assumes First Come First Served (FCFS) queue discipline. Secondly, PathSimR assumes that once a patient joins a queue, they will not leave it until they are served at the associated service point (i.e. PathSimR does not allow reneging or baulking).</p>"
            )
          )
        )
      ),
      tabPanel(
        "Wizard & Setup Terms",
        h3("Wizard & Setup"),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Time Units")),
            align =
              "center"
          ),
          column(
            4,
            p(
              " PathSimR does not have a prescribed time unit, instead users can use whichever time unit is appropriate. This must however be consistent throughout the tool and all data
                           entered must match the chosen units (e.g. all data included is on the scale of days: Arrivals per day, Length of Service on scale of days, prescribed delays in fractions of days)."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Service Point")),
            align =
              "center"
          ),
          column(
            4,
            p(
              " Service Point is a ward, clinic or any treatment/service that occurs on the pathway. This can range from a GP surgery on a set timetable to a bedded ward providing continuous care.
               The key defining feature of a service point is that it has an associated capacity and service time."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Exit")), align = "center"),
          column(
            4,
            p(
              " An exit is any location/service where patients are no longer tracked,
               i.e. they have left the pathway of interest. Example exits could be home, care home, death, another pathway that is not being modelled (e.g. 'Further Treatment', 'Out of patch'').
                              These locations have no associated capacity or LoS and are simply end points along the patient pathway."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Length of Service")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "For the purposes of PathSimR, the Length of Service (or Active Service) corresponds to the amount of time a patient is actually receiving treatment or using a service.
                                          It does not include any time that the patient is blocked or delayed due to capacity restraints nor any prescribed delays.
                                          It represents the time between a patient starting to recieve treatment and the earliest time when they could move on, i.e. the required service time.
                                          This is different to the Length of Stay which is a metric calculated as part of the simulation and includes both active treatment time and any delays which may result due to the dynamics of the system."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Distributions and Parameters"
          )), align = "center"),
          column(
            4,
            p(
              "A probability distribution is a mathematical function that quantifies the likelihood of different possible outcomes. In PathSimR, these outcomes are lengths of time
                               representing service lengths, inter-arrival times and departure delays. Rather than every patient having a length of service equal to the mean, PathSimR relies on probability distributions to help capture the natural variation in the data.
                               There are a large variety of distributions that can be used in modelling, each of which requires different parameters to shape the probabilities."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Transition Delay")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "Transition  delays are included in PathSimR to help simulate an expected amount of time that a patient will remain in a ward, clinic, or other service point after they have completed their treatment before they can move to their next service point or be discharged (to represent, for example, travel time, the completion of administrative tasks, sourcing of social care funding, discussions with families, or delays in discharge to services which are not being explicitly modelled).
                                          Delays can occur between any pair of service points (that have a zero length queue between them) or between a service point and an exit point.
                                          The delay operates by keeping a patient in their current location while holding a space for them in their onward location (when exiting the pathway, there is always space).
                                          This is summarised in the Transition Delay tab in the outputs."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Capacity Driven Delay"
          )), align = "center"),
          column(
            4,
            p(
              "Capacity delays occur as a result of blocking after service and are not inputs into the model. They are listed here to help distinguish them from the Transition Delays.
                               They are a result of lack of downstream capacity within the network (i.e. at a service point with defined capacity) which forces a patient to stay in their current location until a space in an onward service point or queue is available.
                                          This is summarised in the Capacity Driven Delay tab in the outputs."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Warm-up period")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "The warm-up period represents the time it takes for a simulation to reach stable running conditions and after which results can be recorded.
                                          As each simulation starts from empty, it is important that the warm-up period be long enough so that the results collected are reflective of
                                          the modelled system and not an emptier version. "
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Simulation period")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "The Simulation Period is the amount of time over which to collect results from the simulation.
                                          This will need to be sufficiently long such that a large number of patients can pass through the pathway.
                                          For example, if a pathway has an average length of 365 days then simulating it for only 10 would not produce complete results,
                                          as unique patients would not have completed the pathway. The simulation period should therefore be longer than the average pathway
                                          length and long enough to collect data. "
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Replications")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "Number of times to run a particular simulation so as to capture the natural variation in the system.
                                          Results are averaged over all replications to ensure all system eventualities are captured."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Network Input Template"
          )), align = "center"),
          column(
            4,
            p(
              "A .csv file that specifies transition rates between service points, permitted internal and external queue sizes, and the probabilty distribution names and parameters for both service point Lengths of Service, and prescribed transition delays between pairs of servcie points/service points and exits."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Calendar Input Template"
          )), align = "center"),
          column(
            4,
            p(
              "A .csv file that includes the calendar of capacity and mean external arrival rates (by time period) for each service point"
            )
          )
        ),
        br()
      ),
      tabPanel(
        "Output Terms",
        h3("Output Terms"),
        h4("Patient Based Outputs"),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Total time in system (TTIS)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between external arrival and departure to an exit for each patient."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Wait")), align = "center"),
          column(4, p(
            "Time between arrival and service start at a service point (time spent in queue)."
          ))
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Active Service")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "Time between service start and service end at the service point (e.g. treatment on a ward, a clinic appointment etc.)."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Time Delayed (Capacity Driven)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between service end and start of transition delay (or departure if no transition delay) at the service point (e.g. treatment on a ward, a clinic appointment etc.) - the amount of time a patient spends blocked at a service point after completing their Active Service, waiting for capacity to become free downstream."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Time Delayed (Transition)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between capacity driven delay end (or service end if no capacity delay) and departure from the service point (e.g. treatment on a ward, a clinic appointment etc.) - they user defined delay (not depedent on downstream capacity)."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Length of Stay (LOS)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between service start and departure (Active Service + Delay to transfer + Transition delay (user-defined delay))"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Delay To Transfer (DTT)"
          )), align = "center"),
          column(
            4,
            p(
              "Time between service end and departure, i.e. the amount of time the patient is delayed due to a lack of capacity downstream (blocking after service) plus any transition delay."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Rejection Rate")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "Number of patients rejected from full external queues divided by the length of the simulation run."
            )
          )
        ),
        h4("Service Point Based Outputs"),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Queue")), align = "center"),
          column(
            4,
            p(
              "Number of concurrent patients who have arrived at a service point and are yet to start the service."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Occupancy/Patient Occupancy"
          )), align = "center"),
          column(
            4,
            p(
              "Number of patients who are actually receiving or have received service and are occupying a space in the service point."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Bed Occupancy")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "The total number of beds currently not available to new arrivals - includes Occupancy/Patient Occupancy as above and also any 'empty' beds that are currently reserved for patients under Transition Delay upstream."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p(
            "Capacity Driven Delay"
          )), align = "center"),
          column(
            4,
            p(
              "Number of patients concurrently delayed due to insufficient capacity downstream (blocking after service). These patients are included in the occupancy and the bed occupancy"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Transition Delay")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "Number of patients concurrently experiencing a prescribed transfer delay.
                             Patients moving to downstream nodes will also be reserving a space in the onward node and thus appear in the bed occupancy metric for that unit.
                             Patients are included in the occupancy and bed occupancy of the current node"
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("% time at level")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "The percentage of the total simulation time that a service point was at a particular level of the output measure of interest (calculated across all replications) - e.g. if there was a queue of length 5 at service point A was 150 time units out of a total simulation time of 1,500 time units, the '% time at level' for that unit and that queue would be 10%."
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(2, strong(p("Percentiles")),
            align =
              "center"
          ),
          column(
            4,
            p(
              "The percentage of total simulation time that the metric of interest was below (or above) the given level - e.g. if the 95th percentile occupancy for unit A was 6, then it's occupancy was at or below 6 for 95% of the simulation time (and conversely, there it was greater than six for 5% of the simulation time). The 50th percentile is the median."
            )
          )
        ),
        br(),
        h4("Connecting Events with Outputs"),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(
            6,
            p(
              "The figure below shows how the different events that occur at each Service Point connect to the different outputs
                        listed on this page. The outputs in the coloured boxes represent the time spent in those states, e.g. the time
                        between Arrival and Service Start is defined as the Wait. Both Delay-To-Transfer and Length of Stay are combined metrics
                        that represent the sum of time spent in multiple states."
            )
          )
        ),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(
            6,
            p(
              "The Service Point Based Outputs refer to the number of patients concurrently existing in the same state/activity.
                         For example, the number of patients concurrently experiencing Capacity Driven Delay are all those that are between
                         Service End and Transition Start simultaneously."
            )
          )
        ),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(6, img(src = "www/Event_Outputs.png"))
        )
      ),
      widths = c(2, 10),
      well = TRUE
    )
  )
}
