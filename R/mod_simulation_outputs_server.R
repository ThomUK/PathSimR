#' simulation_outputs Server Module
#'
#' @param id Internal parameter for {shiny}.
#' @param sim_out Reactive returning the simulation output list.
#' @param viz Reactive returning the DiagrammeR network graph.
#'
#' @import shiny
#' @noRd
mod_simulation_outputs_server <- function(id, sim_out, viz) {
  moduleServer(id, function(input, output, session) {
    ### RENDER TOTAL TIME IN SYSTEM #####
    logger::log_info("Rendering simulation results.")

    output$ttis <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$total_time_in_system
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong("Total time in system "))
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )

    output$ttiss <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$total_time_in_system_summary

        tmp$metric <- paste0("Total Time In System (", x$time_unit, ")")
        colnames(tmp) <-
          c(
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
        tmp
      },
      rownames = FALSE
    )

    ### RENDER WAITS #####

    output$node_wait <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_wait
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong("Wait"))
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$node_wait_summary <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_wait_summary
        tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]

        tmp$metric <- paste0("Wait (", x$time_unit, ")")
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )


    output$pat_wait <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_wait
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong("Wait"))
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$pat_wait_summary <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_wait_summary

        tmp$metric <- paste0("Wait (", x$time_unit, ")")
        colnames(tmp) <-
          c(
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )


    ### RENDER ACTIVE SERVICE #####

    output$node_active_service <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_active_service
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong("Active Service"))
        ),
        rownames = FALSE,
        filter =
          "top",
        pageLength = 10, dom = "tlp"
      )
    )


    output$node_active_service_summary <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_active_service_summary
        tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]

        tmp$metric <- paste0("Active Service (", x$time_unit, ")")
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )



    output$pat_active_service <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_active_service
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong("Active Service"))
        ),
        rownames = FALSE,
        filter =
          "top",
        pageLength = 10, dom = "tlp"
      )
    )


    output$pat_active_service_summary <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_active_service_summary
        colnames(tmp) <-
          c(
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )



    ### RENDER CAPACITY DELAYS #####

    output$node_capacity_delay <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_capacity_delay
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(
            strong("Time Delayed (Capacity Driven)")
          )
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$node_capacity_delay_summary <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_capacity_delay_summary
        tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]

        tmp$metric <- paste0("Capacity Delay (", x$time_unit, ")")
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )


    output$pat_capacity_delay <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_capacity_delay
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(
            strong("Time Delayed (Capacity Driven)")
          )
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$pat_capacity_delay_summary <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_capacity_delay_summary


        tmp$metric <- paste0("Capacity Delay (", x$time_unit, ")")
        colnames(tmp) <-
          c(
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )


    ### RENDER TRANSITION DELAYS #####

    output$node_transition_delay <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_transition_delay
        tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Time Delayed (Transition)"
          ))
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$node_transition_delay_summary <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_transition_delay_summary

        tmp$metric <- paste0("Transition Delay (", x$time_unit, ")")
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )


    output$pat_transition_delay <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_transition_delay
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Time Delayed (Transition)"
          ))
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$pat_transition_delay_summary <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_capacity_delay_summary

        tmp$metric <- paste0("Transition Delay (", x$time_unit, ")")
        colnames(tmp) <-
          c(
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )



    ### RENDER LENGTH OF STAY #####

    output$node_los <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_length_of_stay
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong("Length of Stay"))
        ),
        rownames = FALSE,
        filter =
          "top",
        pageLength = 10, dom = "tlp"
      )
    )


    output$node_loss <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_length_of_stay_summary
        tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]

        tmp$metric <- paste0("Length Of Stay (", x$time_unit, ")")
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )


    output$pat_los <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_length_of_stay
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong("Length of Stay"))
        ),
        rownames = FALSE,
        filter =
          "top",
        pageLength = 10, dom = "tlp"
      )
    )


    output$pat_loss <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_length_of_stay_summary

        tmp$metric <- paste0("Length Of Stay (", x$time_unit, ")")
        colnames(tmp) <-
          c(
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )



    ### RENDER DELAY TO TRANSFER #####

    output$node_dtt <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_delay_to_transfer
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Delay to Transfer"
          ))
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$node_dtts <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$node_delay_to_transfer_summary
        tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]

        tmp$metric <- paste0("Delay To Transfer (", x$time_unit, ")")
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )



    output$pat_dtt <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_delay_to_transfer
        tmp <- data.table::rbindlist(tmp)
        # tmp<-format(tmp,digits=5)
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Delay to Transfer"
          ))
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$pat_dtts <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pat_delay_to_transfer_summary

        tmp$metric <- paste0("Delay To Transfer (", x$time_unit, ")")
        colnames(tmp) <-
          c(
            "Metric",
            "Mean",
            "Standard Deviation",
            "IQR",
            "95th Percentile"
          )
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )


    ### RENDER REJECTION RATE #####


    output$rejs <- renderTable(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$rejected_summary
        tmp <- tmp[order(factor(x = tmp$node, levels = x$syst_names[, 2])), ]
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <- c("Service Point", "Mean")
        tmp <- format(tmp, digits = 4, scientific = F)
      },
      rownames = FALSE
    )


    ### RENDER DELAY METRICS #####


    output$ptd_percent <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$ptd_percent
        tmp <- format(tmp, digits = 4, scientific = F)
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Delayed Level",
            "% time at Delayed Level",
            "Cumulative % time at or below Delayed Level"
          )
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(
            strong("Percentage time at delayed level")
          )
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$ptd_plot <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$ptd_plot
        tmp
      },
      res = 128
    )


    output$avg_delayed <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$avg_delayed_summary
        tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")
        # tmp<-data.table::rbindlist(tmp)
        tmp[, 2] <- format(tmp[, 2], digits = 5)
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Average # Delayed"
          ))
        ),
        rownames = FALSE,
        pageLength = 10, dom = "tlp"
      )
    )


    output$d <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$d
        tmp
      },
      res = 128
    )

    ### RENDER QUEUE METRICS #####

    output$ptq_percent <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$ptq_percent
        tmp <- format(tmp, digits = 4, scientific = F)
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Queue Length",
            "% time at Queue Length",
            "Cumulative % time at or below Queue Length"
          )
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(
            strong("Percentage time at queue length")
          )
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$ptq_plot <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$ptq_plot
        tmp
      },
      res = 128
    )


    output$avg_queue <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$avg_queue_summary
        tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")
        # tmp<-data.table::rbindlist(tmp)
        tmp[, 2] <- format(tmp[, 2], digits = 5)
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Average queue length"
          ))
        ),
        rownames = FALSE,
        pageLength = 10, dom = "tlp"
      )
    )


    output$q <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$q
        tmp
      },
      res = 128
    )

    ### RENDER OCCUPANCY METRICS #####

    output$pto_percent <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pto_percent
        tmp <- format(tmp, digits = 4, scientific = F)
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Patient Occupancy Level",
            "% time at Patient Occupancy Level",
            "Cumulative % time at or below Patient Occupancy Level"
          )
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(
            strong("Percentage time at occupancy level")
          )
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$pto_plot <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$pto_plot
        tmp
      },
      res = 128
    )


    output$avg_occupancy <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$avg_occupancy_summary
        tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")
        # tmp<-data.table::rbindlist(tmp)
        tmp[, 2] <- format(tmp[, 2], digits = 5)
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Average Occupancy"
          ))
        ),
        rownames = FALSE,
        pageLength = 10, dom = "tlp"
      )
    )


    output$o <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$o
        tmp
      },
      res = 128
    )

    ### RENDER TRANSITION METRICS #####

    output$ptt_percent <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$ptt_percent
        tmp <- format(tmp, digits = 4, scientific = F)
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Transition Level",
            "% time at Transition Level",
            "Cumulative % time at or below Transition Level"
          )
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(
            strong("Percentage time at transition level")
          )
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$ptt_plot <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$ptt_plot
        tmp
      },
      res = 128
    )


    output$avg_transition <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$avg_transition_summary
        tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")
        # tmp<-data.table::rbindlist(tmp)
        tmp[, 2] <- format(tmp[, 2], digits = 5)
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Average Transition"
          ))
        ),
        rownames = FALSE,
        pageLength = 10, dom = "tlp"
      )
    )


    output$t <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$t
        tmp
      },
      res = 128
    )

    ### RENDER BED OCCUPANCY METRICS #####

    output$ptb_percent <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$ptb_percent
        tmp <- format(tmp, digits = 4, scientific = F)
        tmp$node <- str_replace_all(tmp$node, pattern = "_", replacement = " ")
        colnames(tmp) <-
          c(
            "Service Point",
            "Bed Occupancy Level",
            "% time at Bed Occupancy Level",
            "Cumulative % time at or below Bed Occupancy Level"
          )
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(
            strong("Percentage time at occ_bed level")
          )
        ),
        rownames = FALSE,
        filter = "top",
        pageLength = 10, dom =
          "tlp"
      )
    )


    output$ptb_plot <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$ptb_plot
        tmp
      },
      res = 128
    )


    output$avg_occ_bed <- DT::renderDT(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$avg_occ_bed_summary
        tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")
        # tmp<-data.table::rbindlist(tmp)
        tmp[, 2] <- format(tmp[, 2], digits = 5)
        tmp
      },
      options = list(
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Average Bed Occupanncy"
          ))
        ),
        rownames = FALSE,
        pageLength = 10, dom = "tlp"
      )
    )


    output$b <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$b
        tmp
      },
      res = 128
    )


    ### RENDER MULTIPLOT #####

    output$multi_plot <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$avg_through_time_plot
        tmp
      },
      res = 175
    )

    ### RENDER Warm-Up Assistance Plot #####

    output$tisp <- renderPlot(
      {
        req(sim_out())
        x <- sim_out()
        tmp <- x$tisp
        tmp
      },
      res = 175
    )


    ### RENDER PERCENTILE TABLES #####

    output$dpercentiles <- DT::renderDT({
      req(sim_out())

      sketch <- htmltools::withTags(table(
        class = "display",
        thead(
          tr(
            th(rowspan = 2, "Service Point"),
            th(colspan = 7, "Percentiles")
          ),
          tr(lapply(
            c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
            th
          ))
        )
      ))


      x <- sim_out()
      tmp <- x$dpercentiles
      tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")
      # tmp<-ceiling(tmp)
      # tmp<-type.convert(tmp)
      # tmp<-format(tmp,digits=5)
      DT::datatable(
        tmp,
        container = sketch,
        options = list(dom = "t", ordering = F),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Delay Percentiles"
          ))
        )
      )
    })


    output$qpercentiles <- DT::renderDT({
      req(sim_out())

      sketch <- htmltools::withTags(table(
        class = "display",
        thead(
          tr(
            th(rowspan = 2, "Service Point"),
            th(colspan = 7, "Percentiles")
          ),
          tr(lapply(
            c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
            th
          ))
        )
      ))


      x <- sim_out()
      tmp <- x$qpercentiles
      tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")
      # tmp<-ceiling(tmp)
      # tmp<-type.convert(tmp)
      # tmp<-format(tmp,digits=5)
      DT::datatable(
        tmp,
        container = sketch,
        options = list(dom = "t", ordering = F),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Queue Percentiles"
          ))
        )
      )
    })


    output$opercentiles <- DT::renderDT({
      req(sim_out())

      sketch <- htmltools::withTags(table(
        class = "display",
        thead(
          tr(
            th(rowspan = 2, "Service Point"),
            th(colspan = 7, "Percentiles")
          ),
          tr(lapply(
            c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
            th
          ))
        )
      ))


      x <- sim_out()
      tmp <- x$opercentiles
      # format means you need to use matrix referencing without names here, rather than dollar sign/name referencing
      tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")

      # tmp<-ceiling(tmp)
      # tmp<-type.convert(tmp)
      # tmp<-format(tmp,digits=5)
      DT::datatable(
        tmp,
        container = sketch,
        options = list(dom = "t", ordering = F),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Occupancy Percentiles"
          ))
        )
      )
    })

    output$bpercentiles <- DT::renderDT({
      req(sim_out())

      sketch <- htmltools::withTags(table(
        class = "display",
        thead(
          tr(
            th(rowspan = 2, "Service Point"),
            th(colspan = 7, "Percentiles")
          ),
          tr(lapply(
            c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
            th
          ))
        )
      ))


      x <- sim_out()
      tmp <- x$bpercentiles
      tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")
      # tmp<-ceiling(tmp)
      # tmp<-type.convert(tmp)
      # tmp<-format(tmp,digits=5)
      DT::datatable(
        tmp,
        container = sketch,
        options = list(dom = "t", ordering = F),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Bed Occupancy Percentiles"
          ))
        )
      )
    })



    output$tpercentiles <- DT::renderDT({
      req(sim_out())

      sketch <- htmltools::withTags(table(
        class = "display",
        thead(
          tr(
            th(rowspan = 2, "Service Point"),
            th(colspan = 7, "Percentiles")
          ),
          tr(lapply(
            c("50th", "80th", "85th", "90th", "95th", "99th", "100th"),
            th
          ))
        )
      ))


      x <- sim_out()
      tmp <- x$tpercentiles
      tmp[, 1] <- str_replace_all(tmp[, 1], pattern = "_", replacement = " ")
      # tmp<-ceiling(tmp)
      # tmp<-type.convert(tmp)
      # tmp<-format(tmp,digits=5)
      DT::datatable(
        tmp,
        container = sketch,
        options = list(dom = "t", ordering = F),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: center;",
          htmltools::h4(strong(
            "Transition Percentiles"
          ))
        )
      )
    })




    output$tables_viz1 <- DiagrammeR::renderGrViz({
      viz()
    })

    output$tables_viz2 <- DiagrammeR::renderGrViz({
      viz()
    })
  })
}
