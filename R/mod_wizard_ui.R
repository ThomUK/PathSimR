mod_wizard_ui_1 <- function(id) {
  ns <- NS(id)

  #### Creating the starting name matricies ####
  service_points <- matrix(
    nrow = 1,
    ncol = 1,
    data = c("OP Clinic")
  )
  colnames(service_points) <- c("Service Points (enter required names below)")


  exits <- matrix(
    nrow = 1,
    ncol = 1,
    data = c("Home")
  )
  colnames(exits) <- c("Exits (enter required names below)")

  tabPanel(
    "W1. Setup",
    sidebarLayout(
      sidebarPanel(
        h3(strong("Instructions")),
        h4("Step 1: Enter names of all Service Points"),
        p(
          "'OP Clinic' is currently listed as an example Service Point.
               Enter names in the 'Service Point' column by selecting an empty cell or editing an existing one.
                           The entry form will automatically grow when the limit is reached.
                           To refresh, click away and then enter new name.",
          actionLink(
            inputId = ns("serv_point_help"),
            label = "What is a Service Point?",
            icon = icon("info-circle")
          ),
          style = "color:gray"
        ),
        shinyBS::bsModal(
          id = ns("modal_serv_point"),
          title = HTML("<h2><strong>Service Point Help</strong></h2>"),
          trigger = ns("serv_point_help"),
          size = "large",
          ... =
            HTML(
              "
                         <p> A Service Point is a ward, clinic or any treatment/service that occurs on the pathway. This can range from a GP surgery on a set timetable to a bedded ward providing continuous care.
               The key defining feature of a service point is that it has an associated capacity and service time.</p>
                              "
            )
        ),
        br(),
        h4("Step 2: Enter names of all Exits"),
        p(
          "'Home' is currently listed as an example Exit.
               Enter names in the 'Exit' column by selecting an empty cell or editing an existing one.
                           The entry form will automatically grow when the limit is reached.
                           To refresh, click away and then enter new name.",
          actionLink(
            inputId = ns("exit_help"),
            label = "What is an Exit?",
            icon = icon("info-circle")
          ),
          style = "color:gray"
        ),
        shinyBS::bsModal(
          id = ns("modal_exit"),
          title = HTML("<h2><strong>Exit Help</strong></h2>"),
          trigger = ns("exit_help"),
          size = "large",
          ... =
            HTML(
              "
                         <p> An exit is any location/service where patients are no longer tracked,
               i.e. they have left the pathway of interest. Example exits could be home, care home, mortality, another pathway that isn't being modelled (e.g. 'Further Treatment', 'Out of patch'').
                              These locations have no associated capacity or LoS and are simply end points along the patient pathway.</p>
                              "
            )
        ),
        br(),
        h4(
          "Step 3: Check the resulting tables and ensure all entries are included"
        ),
        br(),
        h4("Step 4: Proceed by pressing the 'Next' button."),
        p(
          "If you require to add/remove any names during the wizard process, you can return to this page and edit the inputs
               to restart the wizard.",
          style = "color:gray"
        ),
        br(),
        fluidRow(
          column(
            6,
            align = "center",
            actionButton(
              inputId = ns("jb2i2"),
              label = "Back to Intro",
              icon = icon("arrow-left")
            )
          ),
          column(6, align = "center", actionButton(
            inputId = ns("j2de"), label = c(tagList("Next", icon("arrow-right")))
          ))
        ),
        width = 3
      ),
      mainPanel(
        fluidRow(
          br(),
          fluidRow(h2(strong(
            textOutput(ns("duplicate"))
          )), align = "center"),
          br(),
          br(),
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(
            4,
            shinyMatrix::matrixInput(
              inputId = ns("service_points"),
              value = service_points,
              class = "character",
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
          ),
          column(
            width = 2,
            offset = 0,
            style = "padding:0px;"
          ),
          column(
            4,
            shinyMatrix::matrixInput(
              inputId = ns("exits"),
              value = exits,
              class = "character",
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
          ),
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          )
        ),
        br(),
        fluidRow(
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          ),
          column(4, tableOutput(ns("sp_table")), align = "center"),
          column(
            width = 2,
            offset = 0,
            style = "padding:0px;"
          ),
          column(4, tableOutput(ns("exit_table")), align = "center"),
          column(
            width = 1,
            offset = 0,
            style = "padding:0px;"
          )
        )
      )
    )
  )
}


mod_wizard_ui_2 <- function(id) {
  ns <- NS(id)

  tabPanel(
    "W2. Data Entry",
    sidebarLayout(
      sidebarPanel(
        h3(strong("Instructions")),
        br(),
        h4(strong(
          em("For each named tab on the right, fill out all the information")
        )),
        br(),
        h4(
          "Step 1: Enter a Length of Service distribution and parameters for the Service Point"
        ),
        p(
          "Select the distribution from the drop-down below and then enter the parameter values (as numbers) to the right, in the correct named box.
                 If the distribution and parameters for the service point are not know, use the Service Distribution tool (in the navigation bar above)
                 to either fit models to uploaded data or scale against BNSSG data and then enter resulting distributions and parameters.",
          actionLink(
            inputId = ns("serv_help"),
            label = "What is a Length of Service and how does it connect to distributions and parameters?",
            icon = icon("info-circle")
          ),
          style = "color:gray"
        ),
        shinyBS::bsModal(
          id = ns("modal_serv"),
          title = HTML(
            "<h2><strong>Length of Service & Distributions Help</strong></h2>"
          ),
          trigger = ns("serv_help"),
          size = "large",
          ... =
            HTML(
              "<h3><strong>Length of Service</strong></h3>
                         <p>For the purposes of PathSimR, the Length of Service (or Active Service) corresponds to the amount of time a patient is actually receiving treatment or using a service.
                         It does not include any time that the patient is blocked or delayed due to capacity restraints nor any prescribed delays.
                         It represents the time between a patient starting to recieve treatment and the earliest time when they could move on, i.e. the required service time.
                         This is different to the Length of Stay which is a metric calculated as part of the simulation and includes both active treatment time and any delays which
                         may result due to the dynamics of the system.</p>

                              <h3><strong>LoS Distributions and Parameters</strong></h3>
                              <p> Rather than every patient having a length of service equal to the mean, PathSimR uses probability distributions (range of values with different probabilities) to help capture the natural variation in the data.
                              For example, using only the average LoS would ignore instances where patients have substaintially longer service times, which in turn could impact effective capacity.
                              The probability of having a certain length of service is modelled by the user chosen distributions and parameters. PathSimR includes a Service Distribution Tool, which
                              allows users to either fit models to uploaded data or scale against BNSSG data and is accessed from the Navigation Bar.</p>
                              "
            )
        ),
        br(),
        h4("Step 2: Enter information about maximum queue lengths"),
        p(
          "Enter numberic values into both boxes.",
          actionLink(
            inputId = ns("queue_help"),
            label = "What counts as a queue?",
            icon = icon("info-circle")
          ),
          style = "color:gray"
        ),
        shinyBS::bsModal(
          id = ns("modal_queue"),
          title = HTML("<h2><strong>External & Internal Queue Help</strong></h2>"),
          trigger = ns("queue_help"),
          size = "large",
          ... =
            fluidRow(column(
              11,
              HTML(
                "
                         <p>A queue in PathSimR is simply a part of the pathway in which the patient can wait for the next service point. These queues can be as small or large as required, even
                         effectively infinite (e.g. when patients simply wait at home for the next available appointment and don't wait in a physical queue). In the case where there is effectively
                         unlimited/infinite queueing capacity, the user can enter a large number (e.g. 99999) to represent a queue that will never fill.
                         In PathSimR, queues are defined by the service point in which they enter, therefore if multiple service points (e.g. A, B & C) have patients that move to service point D,
                         the queue capacity would be shared between patients from A, B & C and the capacity would be defined at service point D.
                         There are two different types of queue:External Queues and Internal Queues, which are both described below. A service point can have both types of queue.</p>

                         <h3><strong>External Queues</strong></h3>
                         <p>An external queue is defined as a queue that accepts arrivals from outside of the pathway and therefore only connect to a single service point. A full external queue
                         causes arriving patients to be lost from the system, a metric which is recorded in the outputs.
                         These are the arrivals that determine the external arrival rate, entered in step 4 on this page.
                         In the pathway visualisation later in the tool, these will appear as red arrows.</p>

                          <h3><strong>Internal Queues</strong></h3>
                          <p>An internal queue is one that connects service points within the pathway network.
                          As outlined above, internal queues are defined by the downstream service point so each service point has only 1 internal queue,
                          not a unique queue between each service point. In the case where an internal queue is full, patients will have to wait in the
                          preceeding service point until a space in the queue becomes available.</p>

                          <h3><strong>Why are there two types of queue?</strong></h3>
                          <p>The two types of queues represent different aspects of the system. Setting the external queue length allows the user to manipulate how external arrivals are treated,
                          e.g. whether they can wait until a service space becomes available or, in the case of a zero length external queue, start service if there is a space in the service point or be lost to the system.
                          Internal queues dictate how patients move & wait between service points, with a zero length internal queue representing the requirement for continuous care and will potentially result in capacity
                          driven delays. </p>
                          "
              )
            ))
        ),
        br(),
        h4("Step 3: Enter Transition Proportions and Transition Delays"),
        p(
          strong(
            "Enter a value between and including 0 and 1 in the proportion box to represent the proportion of patients who move to that service point.",
            "All proportion values should sum to 1 on each Service Point."
          ),
          "If there is a Transition Delay associated with the move, select the describing distribution and enter the neccessary parameters.
                 A fixed Transition Delay can be modelled using the uniform distribution and entering the same value into the min and max boxes.",
          actionLink(
            inputId = ns("delay_help"),
            label = "What is a Delay and how are they important?",
            icon = icon("info-circle")
          ),
          style = "color:gray"
        ),
        shinyBS::bsModal(
          id = ns("modal_delay"),
          title = HTML("<h2><strong>Departure Delays Help</strong></h2>"),
          trigger = ns("delay_help"),
          size = "large",
          ... =
            fluidRow(column(
              11,
              HTML(
                "
                         <h3>Transition Delays</h3>
                         <p>
                         Transition Delays are included in PathSimR to help simulate the amount of time needed to move from one unit to the next.
                         Delays can occur between any pair of service points (that have a zero length queue between them) or between a service point and an exit point.
                         The delay functions by keeping a patient in their current location while holding a space for them in their onward location (when exiting the pathway,
                         there is always space). The delays are formulated in the same way as the Length of Services, i.e. using probability distributions
                         to model variation. A fixed Transition Delay can be applied to patient using the uniform distribution and entering the same value
                         into the min and max boxes (e.g. If all patients need to have exactly a 2 day delay when exiting to a care home, select the uniform
                         distribution and enter a 2 into both parameter boxes). The Transition Delay tab in the outputs looks at the number of patients who are concurrently
                         experiencing a trasition delay through time.
                         </p>

                         <h3>Capacity Driven Departure Delays</h3>
                         <p> Even if no Transition Delays are included in the pathway, delays due to capacity can still occur. These delays are due to blocking after service
                         and arise when there is no available capacity in an onward service point and no queue to join. This forces the patient to reside in their current
                         location, stopping new patients from starting. The capacity delay ends when a space becomes available for the patient downstream. The Capacity Driven Delay
                         tab in the outputs looks at the number of patients who are concurrently experiencing a capacity driven delay through time.
                         </p>

                         <h3>What is Delay To Transfer?</h3>
                         <p>There is a output metric called 'Delay To Transfer', which looks at the amount of time between a patient finishing service and departing the service point.
                         This time is the sum of time experiencing any Capacity Driven Delay and any transition delay at a service point. This
                         metric can be found on the statistics output tabs in the Outputs section.
                         </p>

                              "
              )
            ))
        ),
        br(),
        h4("Step 4: Complete the Calendars"),
        p(
          "The External Arrival Rate & Capacity are able to change at given times throughout the simulation. These changes occur at times set in the respective calendars.
                 Both calendars require at least 1 row to be filled.",
          actionLink(
            inputId = ns("cal_help"),
            label = "How do I fill the calendar?",
            icon = icon("info-circle")
          ),
          style = "color:gray"
        ),
        shinyBS::bsModal(
          id = ns("modal_calendar"),
          title = HTML("<h2><strong>Calendar Help</strong></h2>"),
          trigger = ns("cal_help"),
          size = "large",
          ... =
            HTML(
              "<h3><strong>External Arrival Rate Calendar</strong></h3>
                              <p><b><em>Only include arrivals from outside the pathway i.e. those that would join the external queue
                              and have not moved from a Service Point on the pathway.
                              If there is no change in the external arrival rate through time, enter 0 in
                              the start column and the arrival rate (0 if there are no external arrivals) in the Arrival Rate column.</em></b> If the arrival
                              rate does change through time, fill out a row for each period in sequence, matching
                              the end times with the subsequent start times. The simulation will loop through the
                              calendar (i.e. will reach the max end time and then start again from the first
                              calendar entry). </p>
                                <p>The arrival calendar below follows the following pattern:</p>
                                <ul>
                                  <li>Between time 0 and 100, the arrival rate is an average of 1 patient per time step.</li>
                                  <li>Between time 100 and 150, there are no patients arriving.</li>
                                  <li>Between time 150 and 200, the arrival rate is an average of 2 patients per time step.</li>
                                  <li>The calendar then returns to the first row and starts again
                                  (i.e. between time 200 and 300, average arrival rate is 1 patient per time step).</li>
                                  </ul>"
            ),
          br(),
          fluidRow(column(
            12, tableOutput(ns("ext_arr_example")),
            align = "center"
          )),
          HTML(
            "<p>&nbsp;</p>
                              <h3><strong>Capacity Calendar</strong></h3>
                              <p><b><em>If there is no change in the capacity through time, enter 0 in the start
                              column and the capacity in the Capcity column.</b></em> If the capacity does change
                              through time, fill out a row for each period in sequence, matching the end
                              times with the subsequent start times. The simulation will loop through the
                              calendar (i.e. will reach the max end time and then start again from the first calendar entry).
                              If modelling a clinic or a fixed time Service Point, the capacity can be set to 0 for a period
                              of time to represent a closed service.</p>
                              <p>The capacity calendar below follows the following pattern:</p>
                                <ul>
                                  <li>Between time 0 and 30, the capacity is 24 (beds or service spaces).</li>
                                  <li>Between time 30 and 90, the capacity is 48 (beds or service spaces).</li>
                                  <li>Between time 90 and 180, the capacity is 72 (beds or service spaces).</li>
                                  <li>The calendar then returns to the first row and starts again
                                  (i.e. between time 180 and 210, the capacity is 24).</li>
                                  </ul>
                              <p>&nbsp;</p>"
          ),
          fluidRow(column(12, tableOutput(ns("cap_example")),
            align =
              "center"
          ))
        ),
        br(),
        h4("Step 5: Repeat Steps 1 to 4 for each Service Point Tab"),
        p(" ", style = "color:gray"),
        br(),
        h4(
          "Step 6: Once all Service Point Tabs are complete, proceed by pressing the 'Next' button"
        ),
        p(
          " A new tab will also appear at the top of the page. If you require to edit any data entered on any tab during the wizard process, you can return to this page and edit the inputs.
                 If you do, then ensure that the subsequent pages are refreshed.",
          style = "color:gray"
        ),
        br(),
        fluidRow(
          column(
            6,
            align = "center",
            actionButton(
              inputId = ns("j2s"),
              label = "Previous",
              icon = icon("arrow-left")
            )
          ),
          column(6, align = "center", actionButton(
            inputId = ns("j2ftd"), label = c(tagList("Next", icon("arrow-right")))
          ))
        ),
        width = 3
      ),
      mainPanel(uiOutput(ns("tabs")))
    )
  )
}


mod_wizard_ui_3 <- function(id) {
  ns <- NS(id)

  tabPanel(
    "W3. Final Wizard Tables & Download",
    sidebarLayout(
      sidebarPanel(
        h3(strong("Instructions")),
        h4(
          "Step 1:  Press the 'Create/Refresh tables' button to see a summary of the data entered & Issues Log."
        ),
        p(
          "There are 4 tables: Issues, Mean Length of Service (only appears when no issues), Network Template and Calendar template",
          style = "color:gray"
        ),
        br(),
        h4(
          "Step 2: If there are any issues, return to the previous page and ammend the data inputs."
        ),
        p(
          "The location of the issue is listed along with a brief description.",
          style = "color:gray"
        ),
        br(),
        h4(
          "Step 3: Once there are no issues remaining, the option to download the templates for further use becomes available"
        ),
        p(
          "The templates created in the wizard can be saved down and then directly used in PathSimR at a later date.
                 Both templates are required for use in this way.",
          style = "color:gray"
        ),
        br(),
        h4("Step 4: Proceed by pressing the 'Move to Simulation Tool' button."),
        p(
          "The inputs created in the wizard can be pulled through on the following page",
          style = "color:gray"
        ),
        br(),
        fluidRow(column(
          12,
          align = "center",
          actionButton(
            inputId = ns("go"),
            label = "Create / Refresh tables",
            style = "padding:10px; font-size:150%"
          )
        )),
        br(),
        uiOutput(ns("download_buttons")),
        br(),
        br(),
        fluidRow(
          column(
            6,
            align = "center",
            actionButton(
              inputId = ns("jb2de"),
              label = "Previous",
              icon = icon("arrow-left")
            )
          ),
          uiOutput(ns("j2st"))
        ),
        width = 3
      ),
      mainPanel(
        fluidRow(
          br(),
          column(12, tableOutput(ns("issues")), align = "center")
        ),
        fluidRow(
          br(),
          column(12, tableOutput(ns("means")), align = "center")
        ),
        fluidRow(
          br(),
          column(12, tableOutput(ns("var_view")), align = "center")
        ),
        fluidRow(
          br(),
          column(12, tableOutput(ns("cal_view")), align = "center")
        )
      )
    )
  )
}
