ui_tab_wizard_1 <- function() {
  #### Creating the starting name matricies ####
  m1 <- matrix(
    nrow = 1,
    ncol = 1,
    data = c("A")
  )
  colnames(m1) <- c("Service Points")
  rownames(m1) <- c("Enter Names in Right Column")


  m2 <- matrix(
    nrow = 1,
    ncol = 1,
    data = c("B")
  )
  colnames(m2) <- c("Exits")
  rownames(m2) <- c("Enter Names in Right Column")

  tabPanel(
    "W1. Setup",
    sidebarLayout(
      sidebarPanel(
        h3(strong("Instructions")),
        h4("Step 1: Enter names of all Service Points"),
        p(
          "'A' is currently listed as an example Service Point.
               Enter names in the 'Service Point' column by selecting an empty cell or editing an existing one.
                           The entry form will automatically grow when the limit is reached.
                           To refresh, click away and then enter new name.",
          actionLink(
            inputId = "serv_point_help",
            label = "What is a Service Point?",
            icon = icon("info-circle")
          ),
          style = "color:gray"
        ),
        bsModal(
          id = "modal_serv_point",
          title = HTML("<h2><strong>Service Point Help</strong></h2>"),
          trigger = "serv_point_help",
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
          "'B' is currently listed as an example Exit.
               Enter names in the 'Exit' column by selecting an empty cell or editing an existing one.
                           The entry form will automatically grow when the limit is reached.
                           To refresh, click away and then enter new name.",
          actionLink(
            inputId = "exit_help",
            label = "What is an Exit?",
            icon = icon("info-circle")
          ),
          style = "color:gray"
        ),
        bsModal(
          id = "modal_exit",
          title = HTML("<h2><strong>Exit Help</strong></h2>"),
          trigger = "exit_help",
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
              inputId = "jb2i2",
              label = "Back to Intro",
              icon = icon("arrow-left")
            )
          ),
          column(6, align = "center", actionButton(
            inputId = "j2de", label = c(tagList("Next", icon("arrow-right")))
          ))
        ),
        width = 3
      ),
      mainPanel(
        fluidRow(
          br(),
          fluidRow(h2(strong(
            textOutput("duplicate")
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
            matrixInput(
              inputId = "sp",
              value = m1,
              class = "character",
              cols = list(
                names = TRUE,
                extend = FALSE,
                editableNames = FALSE
              ),
              rows = list(
                names = TRUE,
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
            matrixInput(
              inputId = "exit",
              value = m2,
              class = "character",
              cols = list(
                names = TRUE,
                extend = FALSE,
                editableNames = FALSE
              ),
              rows = list(
                names = TRUE,
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
          column(4, tableOutput("sp_table"), align = "center"),
          column(
            width = 2,
            offset = 0,
            style = "padding:0px;"
          ),
          column(4, tableOutput("exit_table"), align = "center"),
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
