
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PathSimR

<!-- badges: start -->
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

## Report template files

The *PathSimR_Report.Rmd* and the *template.docx* files are called from
within the app to create a downloadable Rmarkdown report and do not need
to be opened or run separately unless you wish to alter the report
format (e.g. replace the Word template with one containing your own
organisation’s branding).
