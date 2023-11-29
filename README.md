
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PathSimR

<!-- badges: start -->
<!-- badges: end -->

This is a fork of a project from
<https://github.com/nhs-bnssg-analytics/PathSimR>

I am aiming to refactor & modularise the {shiny} code, better understand
how the app works, and potentially convert it to use {simmer}.

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

To use the tool, follow the instructions on the screen.

The *PathSimR_Report.Rmd* and the *template.docx* files are called from
within the app to create a downloadable Rmarkdown report and do not need
to be opened or run separately unless you wish to alter the report
format (e.g. replace the Word template with one containing your own
organisation’s branding).

The remaining folders contain supplementary material to help with the
use of PathSimR:

1.  *documentation* contains the technical documentation for the app and
    a sample use case template to capture modelling requirements

2.  *Use Case Library* contains expamples of requirements, inputs, and
    outputs for four case studies, as an illustration of how the tool
    can be used

3.  *network_templates* contains sample input templates which can be
    loaded into the tool (as an alternative to using the inbuilt input
    wizard). They are intended to demonstrate features of the input
    format and not to represent real pathways. Files in this format can
    be downloaded, modified, and re-uploaded to PathSimR to save time,
    after an initial pathway has been created using the input wizard. A
    simple example of pair of input templates which will create an error
    are also included in this folder, as an aid to identifying problem
    inputs.
