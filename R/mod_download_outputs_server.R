#' download_outputs Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param sim_out Reactive returning the simulation output list.
#'
#' @import shiny
#' @noRd
mod_download_outputs_server <- function(id, sim_out) {
  moduleServer(id, function(input, output, session) {
    ### XLSX DOWNLOAD HANDLER #####

    output$downloadtables <- downloadHandler(
      filename = function() {
        paste0("Simulation Tables.xlsx")
      },
      content = function(filename) {
        req(sim_out())
        x <- sim_out()

        showModal(modalDialog(
          title = div("Tables Rendering", style = "font-size:200%"),
          div("On completion, click anywhere to continue", style = "font-size:200%"),
          easyClose = FALSE,
          footer = NULL,
          size = "l"
        ))

        list_of_datasets <-
          list(
            "total_time_in_system" = x$total_time_in_system,
            "total_time_in_system_summary" = x$total_time_in_system_summary,
            "pat_rep_summary" = x$pat_rep_summary,
            "pat_total_summary" = x$pat_total_summary,
            "node_wait" = x$node_wait,
            "node_wait_summary" = x$node_wait_summary,
            "node_active_service" = x$node_active_service,
            "node_active_service_summary" = x$node_active_service_summary,
            "node_capacity_delay" = x$node_capacity_delay,
            "node_capacity_delay_summary" = x$node_capacity_delay_summary,
            "node_transition_delay" = x$node_transition_delay,
            "node_transition_delay_summary" = x$node_transition_delay_summary,
            "node_length_of_stay" = x$node_length_of_stay,
            "node_length_of_stay_summary" = x$node_length_of_stay_summary,
            "node_delay_to_transfer" = x$node_delay_to_transfer,
            "node_delay_to_transfer_summary" = x$node_delay_to_transfer_summary,
            "rejected_summary" = x$rejected_summary,
            "ptd_percent" = x$ptd_percent,
            "dpercentiles" = x$dpercentiles,
            "avg_delayed_summary" = x$avg_delayed_summary,
            "ptq_percent" = x$ptq_percent,
            "qpercentiles" = x$qpercentiles,
            "avg_queue_summary" = x$avg_queue_summary,
            "pto_percent" = x$pto_percent,
            "opercentiles" = x$opercentiles,
            "avg_occupancy_summary" = x$avg_occupancy_summary,
            "ptb_percent" = x$ptb_percent,
            "bpercentiles" = x$bpercentiles,
            "avg_occ_bed_summary" = x$avg_occ_bed_summary,
            "ptt_percent" = x$ptt_percent,
            "tpercentiles" = x$tpercentiles,
            "avg_transition_summary" = x$avg_transition_summary,
            "avg_through_time_uniform" = x$avg_through_time
          )

        openxlsx::write.xlsx(x = list_of_datasets, file = filename)

        showModal(modalDialog(
          title = div("Tables Download Complete", style = "font-size:200%"),
          div("Click anywhere to continue", style = "font-size:200%"),
          easyClose = TRUE,
          footer = NULL,
          size = "l"
        ))
      }
    )

    ### PLOT DOWNLOAD HANDLER #####
    output$downloadplot <- downloadHandler(
      filename = "Plots.pdf",
      content = function(file) {
        req(sim_out())

        showModal(modalDialog(
          title = div("Plots Rendering", style = "font-size:200%"),
          div("Plots will open in the default PDF reader from which they will need to be saved directly", style = "font-size:200%"),
          easyClose = FALSE,
          footer = NULL,
          size = "l"
        ))

        x <- sim_out()

        pdf(
          file = file,
          width = 14,
          height = 7
        )
        print(x$pto_plot)
        print(x$ptb_plot)
        print(x$ptd_plot)
        print(x$ptt_plot)
        print(x$ptq_plot)
        print(x$avg_through_time_plot)
        print(x$o)
        print(x$b)
        print(x$d)
        print(x$t)
        print(x$q)

        dev.off()

        showModal(modalDialog(
          title = div("Plot Download Complete", style = "font-size:200%"),
          div("Plots will open in the default PDF reader from which they will need to be saved directly. Click anywhere to continue", style = "font-size:200%"),
          easyClose = TRUE,
          footer = NULL,
          size = "l"
        ))
      }
    )

    ### RMARKDOWN DOWNLOAD HANDLER #####
    output$downloadreport <- downloadHandler(
      filename = paste0("PathSimR_Report.docx"),
      content = function(file) {
        showModal(modalDialog(
          title = div("Report Compiling", style = "font-size:200%"),
          div("On completion, click anywhere to continue", style = "font-size:200%"),
          easyClose = FALSE,
          footer = NULL,
          size = "l"
        ))

        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport1 <- file.path(tempdir(), "PathSimR_Report.Rmd")
        tempReport2 <- file.path(tempdir(), "template.docx")

        file.copy(app_sys("app/PathSimR_Report.Rmd"), tempReport1, overwrite = TRUE)
        file.copy(app_sys("app/template.docx"), tempReport2, overwrite = TRUE)
        x <- sim_out()

        # Set up parameters to pass to Rmd document
        params <- list(
          total_time_in_system = x$total_time_in_system,
          total_time_in_system_summary = x$total_time_in_system_summary,
          node_wait = x$node_wait,
          node_wait_summary = x$node_wait_summary,
          pat_wait = x$pat_wait,
          pat_wait_summary = x$pat_wait_summary,
          node_active_service = x$node_active_service,
          node_active_service_summary = x$node_active_service_summary,
          pat_active_service = x$pat_active_service,
          pat_active_service_summary = x$pat_active_service_summary,
          node_length_of_stay = x$node_length_of_stay,
          node_length_of_stay_summary = x$node_length_of_stay_summary,
          pat_length_of_stay = x$pat_length_of_stay,
          pat_length_of_stay_summary = x$pat_length_of_stay_summary,
          node_delay_to_transfer = x$node_delay_to_transfer,
          node_delay_to_transfer_summary = x$node_delay_to_transfer_summary,
          pat_delay_to_transfer = x$pat_delay_to_transfer,
          pat_delay_to_transfer_summary = x$pat_delay_to_transfer_summary,
          pat_rep_summary = x$pat_rep_summary,
          pat_total_summary = x$pat_total_summary,
          ptd_percent = x$ptd_percent,
          ptd_plot = x$ptd_plot,
          avg_delayed = x$avg_delayed,
          avg_delayed_summary = x$avg_delayed_summary,
          d = x$d,
          ptq_percent = x$ptq_percent,
          ptq_plot = x$ptq_plot,
          avg_queue = x$avg_queue,
          avg_queue_summary = x$avg_queue_summary,
          q = x$q,
          pto_percent = x$pto_percent,
          pto_plot = x$pto_plot,
          avg_occupancy = x$avg_occupancy,
          avg_occupancy_summary = x$avg_occupancy_summary,
          o = x$o,
          ptb_percent = x$ptb_percent,
          ptb_plot = x$ptb_plot,
          avg_occ_bed = x$avg_occ_bed,
          avg_occ_bed_summary = x$avg_occ_bed_summary,
          b = x$b,
          ptt_percent = x$ptt_percent,
          ptt_plot = x$ptt_plot,
          avg_transition = x$avg_transition,
          avg_transition_summary = x$avg_transition_summary,
          t = x$t,
          dpercentiles = x$dpercentiles,
          qpercentiles = x$qpercentiles,
          opercentiles = x$opercentiles,
          bpercentiles = x$bpercentiles,
          tpercentiles = x$tpercentiles,
          rejected_summary = x$rejected_summary,
          avg_through_time_plot = x$avg_through_time_plot,
          reps = x$reps,
          ptm = x$ptm,
          avg_through_time = x$avg_through_time,
          nodes = x$nodes,
          warm_up = x$warm_up,
          sim_time = x$sim_time,
          exits = x$exits,
          syst_names = x$syst_names,
          delay_list = x$delay_list,
          cap_cal_input = x$cap_cal_input,
          arr_cal_input = x$arr_cal_input,
          node_capacity_delay = x$node_capacity_delay,
          node_capacity_delay_summary = x$node_capacity_delay_summary,
          pat_capacity_delay = x$pat_capacity_delay,
          pat_capacity_delay_summary = x$pat_capacity_delay_summary,
          node_transition_delay = x$node_transition_delay,
          node_transition_delay_summary = x$node_transition_delay_summary,
          pat_transition_delay = x$pat_transition_delay,
          pat_transition_delay_summary = x$pat_transition_delay_summary,

          # add the time unit as a parameter ####
          # need to ensure that it exists in the object x<-sim_out() first
          time_unit = x$time_unit
        )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          tempReport1,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )

        showModal(modalDialog(
          title = div("Report Download Complete", style = "font-size:200%"),
          div("Click anywhere to continue", style = "font-size:200%"),
          easyClose = TRUE,
          footer = NULL,
          size = "l"
        ))
      }
    )
  })
}
