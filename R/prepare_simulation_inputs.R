#' Prepare and validate simulation inputs
#'
#' Derives all secondary data structures needed by the simulation engine from
#' the raw template inputs. This is a pure function with no side-effects: it
#' does not read files, open connections, or call any Shiny functions.
#'
#' @param var_input data.frame. The variable/network template (rows = nodes,
#'   columns = transition probabilities followed by service parameters).
#' @param cal_input data.frame. The calendar template with columns
#'   \code{metric}, \code{node}, \code{start}, \code{end}, \code{value}.
#'   Node column may use character node names; these are converted to numeric
#'   IDs inside this function.
#' @param syst_names matrix. Two-column matrix of (numeric ID, name) for all
#'   nodes, as produced by \code{load_var_input()}.
#' @param warm_up numeric. Warm-up period length (same units as sim_time).
#' @param sim_time numeric. Simulation run length after warm-up.
#'
#' @returns A named list with elements:
#' \describe{
#'   \item{nodes}{integer vector of active service node IDs}
#'   \item{node_names}{matrix of (ID, name) for active nodes, with trailing NA row}
#'   \item{delay_dist}{data.frame of inter-node delay distributions}
#'   \item{delay_param}{data.frame of inter-node delay parameters}
#'   \item{delay_list}{two-column matrix of (from, to) node pairs with delays}
#'   \item{cap_cal_input}{data.frame. Capacity calendar, shifted for warm-up, node IDs numeric}
#'   \item{arr_cal_input}{data.frame. Arrival calendar, shifted for warm-up, node IDs numeric}
#'   \item{cap_cal_input_original}{data.frame. Capacity calendar before warm-up shift}
#'   \item{arr_cal_input_original}{data.frame. Arrival calendar before warm-up shift}
#'   \item{record_scale}{numeric. Scalar used to size pre-allocated record matrix (0.8)}
#'   \item{na_lim}{numeric. NA tolerance limit used in the sim loop (100)}
#'   \item{rpi}{numeric. Rounding precision increment (0.1)}
#'   \item{t.period}{numeric. Total simulation period: warm_up + sim_time}
#' }
#' @noRd
prepare_simulation_inputs <- function(var_input, cal_input, syst_names,
                                      warm_up, sim_time) {

  ### Active service nodes ----------------------------------------------------
  nodes <- as.numeric(rownames(
    var_input[which(rowSums(
      var_input[, 1:which(colnames(var_input) == "serv_dist") - 1],
      na.rm = TRUE
    ) != 0), ]
  ))

  node_names <- syst_names[nodes, ]
  node_names <- rbind(node_names, c(NA, NA))
  rownames(node_names) <- c()

  ### Delay distributions and parameters --------------------------------------
  n <- nrow(var_input)

  delay_dist <- var_input[, (n + 5):(n + n + 4)]
  rownames(delay_dist) <- 1:nrow(delay_dist)
  colnames(delay_dist)[1:nrow(delay_dist)] <- c(1:nrow(delay_dist))
  delay_dist[which(delay_dist == "", arr.ind = TRUE)] <- NA

  delay_param <- var_input[, (n + n + 5):ncol(var_input)]
  rownames(delay_param) <- 1:nrow(delay_param)
  colnames(delay_param)[1:nrow(delay_param)] <- c(1:nrow(delay_param))
  delay_param[which(delay_param == "", arr.ind = TRUE)] <- NA

  ### Delay pair list ---------------------------------------------------------
  from <- c(0)
  to   <- c(0)

  for (i in 1:nrow(delay_dist)) {
    for (j in 1:nrow(delay_dist)) {
      if (!is.na(delay_dist[i, j])) {
        from <- c(from, i)
        to   <- c(to, j)
      }
    }
  }

  delay_list <- cbind(from, to)

  ### Convert calendar node names to numeric IDs ------------------------------
  if (!is.null(nrow(node_names))) {
    for (i in 1:nrow(node_names)) {
      cal_input$node[as.character(cal_input$node) == node_names[i, 2]] <-
        as.numeric(i)
    }
  }

  if (is.null(nrow(node_names))) {
    cal_input$node[as.character(cal_input$node) == node_names[2]] <- 1
  }

  ### Split calendar into capacity and arrival --------------------------------
  cap_cal_input <- as.data.frame(cal_input[which(cal_input$metric == "cap"), ])
  arr_cal_input <- as.data.frame(cal_input[which(cal_input$metric == "ext_arr"), ])

  cap_cal_input_original <- cap_cal_input
  arr_cal_input_original <- arr_cal_input

  ### Shift calendars for warm-up ---------------------------------------------
  shifted       <- shift_calendars(cap_cal_input, arr_cal_input, nodes, warm_up)
  cap_cal_input <- shifted$cap_cal_input
  arr_cal_input <- shifted$arr_cal_input

  ### Simulation constants ----------------------------------------------------
  record_scale <- 0.8
  na_lim       <- 100
  rpi          <- 0.1
  t.period     <- warm_up + sim_time

  list(
    nodes                 = nodes,
    node_names            = node_names,
    delay_dist            = delay_dist,
    delay_param           = delay_param,
    delay_list            = delay_list,
    cap_cal_input         = cap_cal_input,
    arr_cal_input         = arr_cal_input,
    cap_cal_input_original = cap_cal_input_original,
    arr_cal_input_original = arr_cal_input_original,
    record_scale          = record_scale,
    na_lim                = na_lim,
    rpi                   = rpi,
    t.period              = t.period
  )
}
