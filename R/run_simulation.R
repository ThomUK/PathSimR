#' Run the PathSimR discrete event simulation
#'
#' @param var_input data.frame. The network/parameter template, with row names
#'   set to 1:n and the node name column removed.
#' @param cal_input data.frame. The calendar template with columns: metric,
#'   node, start, end, value.
#' @param sim_time numeric. Simulation time period (excluding warm-up).
#' @param warm_up numeric. Warm-up period length.
#' @param reps integer. Number of simulation replications.
#' @param syst_names matrix. Two-column matrix of (numeric index, node name string)
#'   for all nodes, including exits.
#' @param syst_names_single character vector. Node name strings only (column 2 of
#'   syst_names).
#' @param time_unit character. Label for time unit used in outputs.
#' @param session Shiny session object. When non-NULL, completion modals are
#'   shown in the UI. Pass NULL (default) to run outside Shiny (e.g. in tests).
#'
#' @returns A named list of 73 simulation output elements.
#' @noRd
run_simulation <- function(var_input, cal_input, sim_time, warm_up, reps,
                           syst_names, syst_names_single,
                           time_unit = "units", session = NULL) {
  ptm <- proc.time()
  cluster <- parallel::makeCluster(
    processor_cores_required(reps)
  )
  on.exit(parallel::stopCluster(cluster), add = TRUE)

  sim_inputs <- prepare_simulation_inputs(
    var_input, cal_input, syst_names,
    warm_up, sim_time
  )
  nodes <- sim_inputs$nodes
  node_names <- sim_inputs$node_names
  delay_dist <- sim_inputs$delay_dist
  delay_param <- sim_inputs$delay_param
  delay_list <- sim_inputs$delay_list
  cap_cal_input <- sim_inputs$cap_cal_input
  arr_cal_input <- sim_inputs$arr_cal_input
  record_scale <- sim_inputs$record_scale
  na_lim <- sim_inputs$na_lim
  rpi <- sim_inputs$rpi
  t.period <- sim_inputs$t.period






  parallel::clusterExport(
    cl = cluster,
    varlist = c(
      "run_single_replication",
      "var_input",
      "syst_names",
      "syst_names_single",
      "nodes",
      "delay_dist",
      "delay_param",
      "delay_list",
      "cap_cal_input",
      "arr_cal_input",
      "record_scale",
      "na_lim",
      "rpi",
      "warm_up",
      "sim_time",
      "t.period",
      "node_names",
      "reps"
    ),
    envir = environment()
  )

  parallel::clusterSetRNGStream(cluster)

  # required to pass magrittr package to the parallel core workers,
  # which cannot be prefixed magrittr:: like other code can
  # TODO refactor to base pipe once tests are in place
  parallel::clusterEvalQ(
    cl = cluster,
    c(
      library(magrittr)
    )
  )


  ####### SIMULATION CODE ##################################################################
  logger::log_debug("Parallel workers starting.")
  outputs <- parallel::parLapply(
    cl = cluster,
    X = 1:reps,
    fun = function(j) {
      run_single_replication(
        j                 = j,
        var_input         = var_input,
        syst_names        = syst_names,
        syst_names_single = syst_names_single,
        nodes             = nodes,
        delay_dist        = delay_dist,
        delay_param       = delay_param,
        delay_list        = delay_list,
        cap_cal_input     = cap_cal_input,
        arr_cal_input     = arr_cal_input,
        record_scale      = record_scale,
        na_lim            = na_lim,
        rpi               = rpi,
        warm_up           = warm_up,
        sim_time          = sim_time,
        t.period          = t.period,
        node_names        = node_names,
        reps              = reps
      )
    }
  )

  logger::log_debug("Processing replication outputs.")
  combo <- process_simulation_outputs(
    outputs = outputs,
    syst_names_single = syst_names_single,
    time_unit = time_unit,
    ptm = ptm,
    cap_cal_input_original = sim_inputs$cap_cal_input_original,
    arr_cal_input_original = sim_inputs$arr_cal_input_original
  )


  # change to check on number of simulation outputs ####
  # this is a manual check to see if every item in "combo", the list of items that the sim_out function
  # will return, has been created. Orignally, this list had 72 items and there was a hard-coded check
  # to see if the length of the list was 72. A further item (the time unit) has now been added, and so
  # the number being checked for is now 73
  # The reason for performing this check, and what it is intended to achieve, needs to be clarified
  if (!is.null(session)) {
    if (length(combo) == 73) {
      logger::log_debug("Simulation complete (combo item is length 73).")
      shiny::showModal(shiny::modalDialog(
        title = div(paste0("Simulation Complete \n(", format(Sys.time()), ")"), style = "font-size:200%"),
        div("Click anywhere on screen to continue", style = "font-size:200%"),
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
    } else {
      logger::log_error("Simulation error.")
      shiny::showModal(shiny::modalDialog(
        title = "Simulation Error",
        "",
        easyClose = TRUE,
        footer = NULL,
        size = "l"
      ))
      shiny::hideTab(inputId = "navbar", target = "3. Simulation Outputs")
      shiny::hideTab(inputId = "navbar", target = "4. Download Outputs")
    }
  }



  return(combo)
}
