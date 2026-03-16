
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PathSimR

<!-- badges: start -->

[![R-CMD-check](https://github.com/ThomUK/PathSimR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ThomUK/PathSimR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This is a fork of a project from
<https://github.com/nhs-bnssg-analytics/PathSimR>

There is a 15 minute video about how to use the tool (made by the BNSSG
team) available here: <https://vimeo.com/643579424>

This fork aims to refactor & modularise the {shiny} code, better
understand how the app works, and potentially convert it in future to
use {simmer}. At the point of forking the app was a single R file over
approx 14K lines long, which I found difficult to study.

## Installation

You can install PathSimR like so:

``` r
# install.packages("remotes")
remotes::install_github("ThomUK/PathSimR")
```

To run the app on your local machine, use the below:

``` r
library("PathSimR")
run_app()
```

or simply:

``` r
PathSimR::run_app()
```

## Using the tool

For a quick overview see the video (made by the BNSSG team) available
here: <https://vimeo.com/643579424>

Broadly, using the tool involves following the instructions on the
screen. There is also documentation available within the PathSimR_Shiny
directory. This includes the folders below:

1.  **Documentation** contains the technical documentation for the app
    and a sample use case template to capture modelling requirements.

2.  **Use Case Library** contains expamples of requirements, inputs, and
    outputs for four case studies, as an illustration of how the tool
    can be used

3.  **Network Templates** contains sample input templates which can be
    loaded into the tool (as an alternative to using the inbuilt input
    wizard). They are intended to demonstrate features of the input
    format and not to represent real pathways. Files in this format can
    be downloaded, modified, and re-uploaded to PathSimR to save time,
    after an initial pathway has been created using the input wizard. A
    simple example of pair of input templates which will create an error
    are also included in this folder, as an aid to identifying problem
    inputs.

## Running the simulation programmatically

The simulation engine can be called directly from R, without launching
the Shiny app. This is useful for scripted analyses, batch runs, or
automated testing.

> **Note:** `run_simulation()` is currently an internal function. Access
> it with `:::` until it is formally exported.

### Step 1 — Prepare inputs

Inputs are two CSVs: a **network template** (node parameters and
transition probabilities) and a **calendar template** (arrival rates and
capacities over time). Sample templates are provided in
`PathSimR_Shiny/network_templates/`.

``` r
network  <- read_network_template("PathSimR_Shiny/network_templates/input_template_1.csv")
calendar <- read_calendar_template("PathSimR_Shiny/network_templates/cal_input_1.csv")
```

### Step 2 — Run the simulation

``` r
result <- PathSimR:::run_simulation(
  var_input         = network$var_input,
  cal_input         = calendar,
  sim_time          = 365,
  warm_up           = 50,
  reps              = 100,
  syst_names        = network$syst_names,
  syst_names_single = network$syst_names_single,
  time_unit         = "days"
)
```

### Step 3 — Access results

`run_simulation()` returns a named list of 73 elements. Key outputs:

``` r
# Mean wait time per node across all replications
result$node_wait_summary

# Mean length of stay per node
result$node_length_of_stay_summary

# Percentage of time each node is occupied (and percentile breakdown)
result$pto_percent
result$opercentiles

# Average occupancy per node
result$avg_occupancy_summary

# Total time patients spend in the system
result$total_time_in_system_summary

# Summary across patients and replications
result$pat_total_summary
```

## Report template files

The *PathSimR_Report.Rmd* and the *template.docx* files are called from
within the app to create a downloadable Rmarkdown report and do not need
to be opened or run separately unless you wish to alter the report
format (e.g. replace the Word template with one containing your own
organisation’s branding).
