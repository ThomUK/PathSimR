tab_service_dist <- function() {
  tabPanel(
    title = "Service Distribution Tool",
    icon = icon("chart-area"),
    sidebarLayout(
      sidebarPanel(
        h3(strong("Instructions")),
        br(),
        actionLink(
          inputId = "model_help",
          label = HTML("Which option do I need?"),
          icon = icon("info-circle"),
          style = " font-size:150%"
        ),
        br(),
        bsModal(
          id = "modal_model",
          title = HTML("<h2><strong>Service Distribution Tool Help</strong></h2>"),
          trigger = "model_help",
          size = "large",
          ... =
            HTML(
              "
                         <p>
                         PathSimR's Service Distribution Tool contains 2 options depending on how much information is available about a service point. </p>

                         <p>If LoS data is available for the service point in question, then <strong>Option 1</strong> should be used. The data can be uploaded and
                         model fits run within the tool that provides the best fitting distribution and parameters that can be used. The data in question must be a single
                         column of data with no column header, saved as a csv. The graph below shows an example of uploaded data with 5 different best fitting distributions
                         plotted to show how the tool approximates the real data.</p>

                         <p>If only the mean LoS is known, then <strong>Option 2</strong> can be used, provided the service point type exists in the library (found on the Scale data by mean tab).
                         This portion of the tool scales a model distribution provided by BNSSG CCG to match the mean provided by the user, resulting in a model that has the correct shape and mean
                         for the service point type in question.
                         </p>
                              "
            ),
          plotOutput("model_help_figure")
        ),
        h4(strong("Option 1: Model fits to user data")),
        h4(
          em("Distribution & Parameters based on User data", style = "color:gray")
        ),
        h5("Step 1: Select the 'Model fits to user data tab"),
        h5(
          "Step 2: Upload a single column csv that only includes LoS data - ",
          em(" No Header required")
        ),
        h5("Step 3: Press the 'Run Distribution Fit Tool' button"),
        h5(
          "Step 4: Inspect the histgram plot and model fit curves, the details of which are displayd in the Ranked Model Table"
        ),
        h5(
          "Step 5: Copy the top ranking model information from the table into the data entry page (i.e. Select the Distribution from the dropdown and enter the parameters listed)"
        ),
        br(),
        h4(strong("Option 2: Scale data by mean")),
        h4(
          em("Distribution & Parameters based on scaled data", style = "color:gray")
        ),
        h5("Step 1: Select the 'Scale data by mean' tab"),
        h5(
          "Step 2: Select a Service Point from the drop-down library that matches the Service Point being modelled"
        ),
        h5(
          "Step 3: Enter the mean LoS associated with the modelled Service Point"
        ),
        h5("Step 4: Press the 'Run/Refresh Scaling Tool' Button"),
        h5(
          "Step 5: Copy the model information from the table into the data entry page (i.e. Select the Distribution from the dropdown and eneter the parameters listed)"
        ),
        h5(
          "Optional Step: Inspect the distribution plot to see a visual version of the Length of Service Distribution"
        ),
        width = 3
      ),
      mainPanel(tabsetPanel(
        tabPanel(
          title = "Model fits to user data",
          fileInput(
            inputId = "los_dat",
            label = "Upload csv",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            ),
            width = "25%"
          ),
          actionButton(inputId = "go_distfit", label = "Run Distribution Fit Tool"),
          br(),
          br(),
          fluidRow(
            column(10, plotOutput("los_fit_plot"), align = "center"),
            column(2, br(), br(), br(), tableOutput("mini_summary"), align = "center")
          ),
          fluidRow(
            br(),
            h3(textOutput("los_text")),
            p(textOutput("los_text_help"))
          ),
          fluidRow(column(12, tableOutput("los_fit_table"),
            align =
              "center"
          )),
          h3(textOutput("fit_error"))
        ),
        tabPanel(
          title = "Scale data by mean",
          fluidRow(column(
            8,
            br(),
            p(
              "Distributions and Parameters have been found for a variety of PODs/ Service Points, which are listed in the Service Point Library.
                            These were based on model fits to BNSSG data in order to match the shape of the Service time distribution. The data is rescaled based
                            on the Average Service value entered to create the required distribution."
            )
          )),
          hr(),
          fluidRow(
            column(3, uiOutput("treatment_select_ui")),
            column(
              2,
              numericInput(
                inputId = "treatment_mean",
                label = "Average Length of Service (Mean)",
                min = 0,
                value = 0,
                step = 0.01
              )
            ),
            column(
              1,
              br(),
              actionButton(inputId = "go_scaled_fit", label = "Run/Refresh Scaling Tool")
            )
          ),
          hr(),
          tableOutput("scaled_fit"),
          plotOutput("scaled_fit_plot")
        )
      ))
    )
  )
}
