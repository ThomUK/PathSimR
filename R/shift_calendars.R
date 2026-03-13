#' Shift capacity and arrival calendars to account for warm-up period
#'
#' When a warm-up period is used, the cyclical calendars need to be rotated so
#' that the position in the cycle at the end of warm-up aligns with time 0 of
#' the reported simulation period.
#'
#' Nodes with a single-row (constant) calendar are passed through unchanged.
#' Nodes where warm_up falls exactly on a cycle boundary are also unchanged.
#'
#' @param cap_cal_input data.frame. Capacity calendar rows from the cal_input
#'   template (metric == "cap"), with numeric node IDs.
#' @param arr_cal_input data.frame. Arrival calendar rows from the cal_input
#'   template (metric == "ext_arr"), with numeric node IDs.
#' @param nodes integer vector. Numeric IDs of active service nodes.
#' @param warm_up numeric. Warm-up period length. If 0, both calendars are
#'   returned unmodified.
#'
#' @returns A named list with elements \code{cap_cal_input} and
#'   \code{arr_cal_input}, each shifted appropriately.
#' @noRd
shift_calendars <- function(cap_cal_input, arr_cal_input, nodes, warm_up) {

  if (warm_up == 0) {
    return(list(cap_cal_input = cap_cal_input,
                arr_cal_input = arr_cal_input))
  }

  ### Shift capacity calendar ------------------------------------------------
  cap_cal_input_new <- cap_cal_input[0, ]

  for (cc in nodes) {
    cap_cal_shift <- cap_cal_input[which(cap_cal_input$node == cc), ]

    if (nrow(cap_cal_shift) > 1) {
      cap_cal_max <- max(cap_cal_shift$end)
      warm_up_modulo <- warm_up %% cap_cal_max

      if (warm_up_modulo != 0) {
        cap_cal_shift$start <- cap_cal_shift$start + warm_up_modulo
        cap_cal_shift$end <- cap_cal_shift$end + warm_up_modulo

        cap_cal_stable <-
          cap_cal_shift[1:min(which(cap_cal_shift$end >= cap_cal_max)), ]
        cap_cal_stable$end[nrow(cap_cal_stable)] <- cap_cal_max

        cap_cal_switch <-
          cap_cal_shift[min(which(cap_cal_shift$end > cap_cal_max)):nrow(cap_cal_shift), ]
        cap_cal_switch$start[1] <- cap_cal_max
        cap_cal_switch$start <- cap_cal_switch$start - cap_cal_max
        cap_cal_switch$end <- cap_cal_switch$end - cap_cal_max

        cap_cal_shift <- rbind(cap_cal_stable, cap_cal_switch)
        cap_cal_shift <- cap_cal_shift[order(cap_cal_shift$start), ]

        cap_cal_input_new <- rbind(cap_cal_input_new, cap_cal_shift)
      } else {
        cap_cal_input_new <- rbind(cap_cal_input_new, cap_cal_shift)
      }
    } else {
      cap_cal_input_new <- rbind(cap_cal_input_new, cap_cal_shift)
    }
  }

  ### Shift arrival calendar -------------------------------------------------
  arr_cal_input_new <- arr_cal_input[0, ]

  for (ac in nodes) {
    arr_cal_shift <- arr_cal_input[which(arr_cal_input$node == ac), ]

    if (nrow(arr_cal_shift) > 1) {
      arr_cal_max <- max(arr_cal_shift$end)
      warm_up_modulo <- warm_up %% arr_cal_max

      if (warm_up_modulo != 0) {
        arr_cal_shift$start <- arr_cal_shift$start + warm_up_modulo
        arr_cal_shift$end <- arr_cal_shift$end + warm_up_modulo

        arr_cal_stable <-
          arr_cal_shift[1:min(which(arr_cal_shift$end > arr_cal_max)), ]
        arr_cal_stable$end[nrow(arr_cal_stable)] <- arr_cal_max

        arr_cal_switch <-
          arr_cal_shift[min(which(arr_cal_shift$end > arr_cal_max)):nrow(arr_cal_shift), ]
        arr_cal_switch$start[1] <- arr_cal_max
        arr_cal_switch$start <- arr_cal_switch$start - arr_cal_max
        arr_cal_switch$end <- arr_cal_switch$end - arr_cal_max

        arr_cal_shift <- rbind(arr_cal_stable, arr_cal_switch)
        arr_cal_shift <- arr_cal_shift[order(arr_cal_shift$start), ]

        arr_cal_input_new <- rbind(arr_cal_input_new, arr_cal_shift)
      } else {
        arr_cal_input_new <- rbind(arr_cal_input_new, arr_cal_shift)
      }
    } else {
      arr_cal_input_new <- rbind(arr_cal_input_new, arr_cal_shift)
    }
  }

  list(cap_cal_input = cap_cal_input_new,
       arr_cal_input = arr_cal_input_new)
}
