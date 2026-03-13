# Tests for process_simulation_outputs()
#
# Builds the `outputs` list by calling run_single_replication() directly
# (no parallel cluster), then passes it to process_simulation_outputs().
# The snapshot tests in test-run_simulation.R cover the full end-to-end
# numerical correctness; these tests focus on the structural contract of
# process_simulation_outputs() in isolation.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

load_var <- function(filename) {
  path <- testthat::test_path("fixtures", filename)
  var_input <- read.csv(path, header = TRUE, sep = ",")
  syst_names <- cbind(
    as.numeric(1:nrow(var_input)),
    as.character(var_input[, 1])
  )
  var_input <- var_input[, -1]
  rownames(var_input) <- 1:nrow(var_input)
  colnames(var_input)[1:nrow(var_input)] <- c(1:nrow(var_input))
  list(
    var_input = var_input, syst_names = syst_names,
    syst_names_single = syst_names[, 2]
  )
}

load_cal <- function(filename) {
  path <- testthat::test_path("fixtures", filename)
  cal <- read.csv(path, header = TRUE, sep = ",")
  cal$node <- as.character(cal$node)
  cal
}

# Build a minimal outputs list (equivalent to parLapply result) using 3 reps
# of template 3. Suppresses the harmless "node_dat not found" warning from
# the pre-existing rm() call inside run_single_replication().
make_outputs <- function(reps = 3, warm_up = 0, sim_time = 100,
                         var_file = "input_template_3.csv",
                         cal_file = "cal_input_3.csv") {
  v <- load_var(var_file)
  cal <- load_cal(cal_file)
  si <- prepare_simulation_inputs(
    v$var_input, cal, v$syst_names,
    warm_up, sim_time
  )
  rep_args <- list(
    var_input         = v$var_input,
    syst_names        = v$syst_names,
    syst_names_single = v$syst_names_single,
    nodes             = si$nodes,
    delay_dist        = si$delay_dist,
    delay_param       = si$delay_param,
    delay_list        = si$delay_list,
    cap_cal_input     = si$cap_cal_input,
    arr_cal_input     = si$arr_cal_input,
    record_scale      = si$record_scale,
    na_lim            = si$na_lim,
    rpi               = si$rpi,
    warm_up           = warm_up,
    sim_time          = sim_time,
    t.period          = si$t.period,
    node_names        = si$node_names,
    reps              = reps
  )
  set.seed(42)
  outputs <- lapply(seq_len(reps), function(j) {
    suppressWarnings(do.call(run_single_replication, c(list(j = j), rep_args)))
  })
  list(
    outputs = outputs,
    syst_names_single = v$syst_names_single,
    cap_cal_input_original = si$cap_cal_input_original,
    arr_cal_input_original = si$arr_cal_input_original
  )
}


# ---------------------------------------------------------------------------
# Output structure
# ---------------------------------------------------------------------------

test_that("returns a named list of length 73", {
  ctx <- make_outputs()
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "days",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  expect_type(combo, "list")
  expect_length(combo, 73)
})

test_that("combo contains the expected top-level names", {
  ctx <- make_outputs()
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "days",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  key_names <- c(
    "total_time_in_system", "total_time_in_system_summary",
    "node_wait", "node_wait_summary",
    "pat_wait", "pat_wait_summary",
    "node_active_service", "node_active_service_summary",
    "ptd_percent", "ptd_plot",
    "ptq_percent", "ptq_plot",
    "pto_percent", "pto_plot",
    "rejected_summary",
    "avg_through_time_plot",
    "reps", "nodes", "warm_up", "sim_time", "exits", "syst_names",
    "delay_list", "cap_cal_input", "arr_cal_input",
    "time_unit", "ptm"
  )
  expect_true(all(key_names %in% names(combo)))
})


# ---------------------------------------------------------------------------
# Simulation parameters are echoed correctly
# ---------------------------------------------------------------------------

test_that("reps, warm_up, sim_time are echoed correctly in combo", {
  ctx <- make_outputs(reps = 3, warm_up = 0, sim_time = 100)
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "days",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  expect_equal(combo$reps, 3)
  expect_equal(combo$warm_up, 0)
  expect_equal(combo$sim_time, 100)
})

test_that("warm_up value is echoed correctly when non-zero", {
  ctx <- make_outputs(reps = 2, warm_up = 20, sim_time = 100)
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "days",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  expect_equal(combo$warm_up, 20)
})


# ---------------------------------------------------------------------------
# time_unit is applied to metric label columns
# ---------------------------------------------------------------------------

test_that("time_unit label is appended to metric column in node_wait", {
  ctx <- make_outputs()
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "hours",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  expect_true(all(grepl("hours", combo$node_wait$metric)))
  expect_true(all(grepl("hours", combo$node_wait_summary$metric)))
  expect_true(all(grepl("hours", combo$total_time_in_system$metric)))
})


# ---------------------------------------------------------------------------
# Calendars are passed through to combo unchanged
# ---------------------------------------------------------------------------

test_that("cap_cal_input and arr_cal_input in combo equal the originals", {
  ctx <- make_outputs()
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "days",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  expect_identical(combo$cap_cal_input, ctx$cap_cal_input_original)
  expect_identical(combo$arr_cal_input, ctx$arr_cal_input_original)
})


# ---------------------------------------------------------------------------
# Summary data frames have the expected columns
# ---------------------------------------------------------------------------

test_that("node_wait_summary has node, metric, mean, sd, iqr, percentile_95 columns", {
  ctx <- make_outputs()
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "days",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  expect_true(all(c("node", "metric", "mean", "sd", "iqr", "percentile_95") %in%
    colnames(combo$node_wait_summary)))
  expect_true(all(c("metric", "mean", "sd", "iqr", "percentile_95") %in%
    colnames(combo$pat_wait_summary)))
})

test_that("rejected_summary has node and mean columns with non-negative values", {
  ctx <- make_outputs()
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "days",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  expect_true(all(c("node", "mean") %in% colnames(combo$rejected_summary)))
  expect_true(all(combo$rejected_summary$mean >= 0, na.rm = TRUE))
})


# ---------------------------------------------------------------------------
# Plot elements are ggplot objects
# ---------------------------------------------------------------------------

test_that("ptd_plot, ptq_plot, pto_plot, avg_through_time_plot are ggplot objects", {
  ctx <- make_outputs()
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "days",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  expect_true(inherits(combo$ptd_plot, "gg"))
  expect_true(inherits(combo$ptq_plot, "gg"))
  expect_true(inherits(combo$pto_plot, "gg"))
  expect_true(inherits(combo$avg_through_time_plot, "gg"))
})


# ---------------------------------------------------------------------------
# time_unit string is stored in combo
# ---------------------------------------------------------------------------

test_that("time_unit string is stored in combo$time_unit", {
  ctx <- make_outputs()
  ptm <- proc.time()

  combo <- process_simulation_outputs(
    outputs = ctx$outputs,
    syst_names_single = ctx$syst_names_single,
    time_unit = "minutes",
    ptm = ptm,
    cap_cal_input_original = ctx$cap_cal_input_original,
    arr_cal_input_original = ctx$arr_cal_input_original
  )

  expect_equal(combo$time_unit, "minutes")
})
