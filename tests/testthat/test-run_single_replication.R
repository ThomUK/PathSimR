# Tests for run_single_replication()
#
# Calls the DES core directly (no parallel cluster) with fixture inputs.
# The snapshot tests in test-run_simulation.R cover end-to-end correctness
# across replications; these tests focus on the contract of a single replication.
#
# Note: the function emits a harmless warning "object 'node_dat' not found"
# from a pre-existing rm() call — this is expected and suppressed below.

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

# Build all inputs needed to call run_single_replication() directly.
# Note: cap_cal_input_original and arr_cal_input_original come from
# prepare_simulation_inputs() but are not parameters of run_single_replication().
make_inputs <- function(var_file, cal_file, warm_up = 0, sim_time = 100, reps = 3) {
  v <- load_var(var_file)
  cal <- load_cal(cal_file)
  si <- prepare_simulation_inputs(
    v$var_input, cal, v$syst_names,
    warm_up, sim_time
  )
  list(
    nodes             = si$nodes,
    node_names        = si$node_names,
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
    var_input         = v$var_input,
    syst_names        = v$syst_names,
    syst_names_single = v$syst_names_single,
    reps              = reps
  )
}

# Wrapper that suppresses the pre-existing "object 'node_dat' not found" warning
# emitted by the rm() cleanup at the end of run_single_replication().
call_rep <- function(...) {
  suppressWarnings(run_single_replication(...))
}


# ---------------------------------------------------------------------------
# Output structure
# ---------------------------------------------------------------------------

test_that("returns a list of length 29 with the expected named elements", {
  inp <- make_inputs("input_template_3.csv", "cal_input_3.csv")

  set.seed(42)
  out <- call_rep(
    j = 1,
    var_input = inp$var_input, syst_names = inp$syst_names,
    syst_names_single = inp$syst_names_single, nodes = inp$nodes,
    delay_dist = inp$delay_dist, delay_param = inp$delay_param,
    delay_list = inp$delay_list, cap_cal_input = inp$cap_cal_input,
    arr_cal_input = inp$arr_cal_input, record_scale = inp$record_scale,
    na_lim = inp$na_lim, rpi = inp$rpi, warm_up = inp$warm_up,
    sim_time = inp$sim_time, t.period = inp$t.period,
    node_names = inp$node_names, reps = inp$reps
  )

  # 27 named results + 2 positionally-accessed elements (cap_cal_input, arr_cal_input)
  expect_type(out, "list")
  expect_length(out, 29)

  named_elements <- c(
    "nodes", "warm_up", "sim_time", "reps", "exits", "syst_names",
    "node_wait", "node_active_service", "node_length_of_stay",
    "node_delay_to_transfer", "pat_wait", "pat_active_service",
    "pat_length_of_stay", "pat_delay_to_transfer", "total_time_in_system",
    "rejected", "ptd", "ptq", "pto", "ptt", "ptb",
    "multi_spread_uniform", "delay_list"
  )
  expect_true(all(named_elements %in% names(out)))
})


# ---------------------------------------------------------------------------
# Inputs are echoed correctly into the output
# ---------------------------------------------------------------------------

test_that("output echoes nodes, warm_up, sim_time, reps correctly", {
  inp <- make_inputs("input_template_3.csv", "cal_input_3.csv",
    warm_up = 10, sim_time = 50, reps = 7
  )

  set.seed(42)
  out <- call_rep(
    j = 1,
    var_input = inp$var_input, syst_names = inp$syst_names,
    syst_names_single = inp$syst_names_single, nodes = inp$nodes,
    delay_dist = inp$delay_dist, delay_param = inp$delay_param,
    delay_list = inp$delay_list, cap_cal_input = inp$cap_cal_input,
    arr_cal_input = inp$arr_cal_input, record_scale = inp$record_scale,
    na_lim = inp$na_lim, rpi = inp$rpi, warm_up = inp$warm_up,
    sim_time = inp$sim_time, t.period = inp$t.period,
    node_names = inp$node_names, reps = inp$reps
  )

  expect_equal(out$nodes, inp$nodes)
  expect_equal(out$warm_up, 10)
  expect_equal(out$sim_time, 50)
  expect_equal(out$reps, 7)
})


# ---------------------------------------------------------------------------
# Summary output data.frames have the expected structure
# ---------------------------------------------------------------------------

test_that("node_wait is a data.frame with the expected columns", {
  inp <- make_inputs("input_template_3.csv", "cal_input_3.csv")

  set.seed(42)
  out <- call_rep(
    j = 1,
    var_input = inp$var_input, syst_names = inp$syst_names,
    syst_names_single = inp$syst_names_single, nodes = inp$nodes,
    delay_dist = inp$delay_dist, delay_param = inp$delay_param,
    delay_list = inp$delay_list, cap_cal_input = inp$cap_cal_input,
    arr_cal_input = inp$arr_cal_input, record_scale = inp$record_scale,
    na_lim = inp$na_lim, rpi = inp$rpi, warm_up = inp$warm_up,
    sim_time = inp$sim_time, t.period = inp$t.period,
    node_names = inp$node_names, reps = inp$reps
  )

  expect_s3_class(out$node_wait, "data.frame")
  expect_true(all(c("node", "metric", "mean", "sd") %in% colnames(out$node_wait)))
  expect_true(all(out$node_wait$mean >= 0, na.rm = TRUE))
})


# ---------------------------------------------------------------------------
# Different seeds produce different results
# ---------------------------------------------------------------------------

test_that("different seeds produce different node_wait means", {
  inp <- make_inputs("input_template_3.csv", "cal_input_3.csv")

  args <- list(
    j = 1,
    var_input = inp$var_input, syst_names = inp$syst_names,
    syst_names_single = inp$syst_names_single, nodes = inp$nodes,
    delay_dist = inp$delay_dist, delay_param = inp$delay_param,
    delay_list = inp$delay_list, cap_cal_input = inp$cap_cal_input,
    arr_cal_input = inp$arr_cal_input, record_scale = inp$record_scale,
    na_lim = inp$na_lim, rpi = inp$rpi, warm_up = inp$warm_up,
    sim_time = inp$sim_time, t.period = inp$t.period,
    node_names = inp$node_names, reps = inp$reps
  )

  set.seed(42)
  out1 <- suppressWarnings(do.call(run_single_replication, args))
  set.seed(9999)
  out2 <- suppressWarnings(do.call(run_single_replication, args))

  expect_false(identical(out1$node_wait$mean, out2$node_wait$mean))
})


# ---------------------------------------------------------------------------
# Deterministic with set.seed
# ---------------------------------------------------------------------------

test_that("same seed produces identical node_wait output on two calls", {
  inp <- make_inputs("input_template_3.csv", "cal_input_3.csv")

  args <- list(
    j = 1,
    var_input = inp$var_input, syst_names = inp$syst_names,
    syst_names_single = inp$syst_names_single, nodes = inp$nodes,
    delay_dist = inp$delay_dist, delay_param = inp$delay_param,
    delay_list = inp$delay_list, cap_cal_input = inp$cap_cal_input,
    arr_cal_input = inp$arr_cal_input, record_scale = inp$record_scale,
    na_lim = inp$na_lim, rpi = inp$rpi, warm_up = inp$warm_up,
    sim_time = inp$sim_time, t.period = inp$t.period,
    node_names = inp$node_names, reps = inp$reps
  )

  set.seed(42)
  out_a <- suppressWarnings(do.call(run_single_replication, args))
  set.seed(42)
  out_b <- suppressWarnings(do.call(run_single_replication, args))

  expect_identical(out_a$node_wait, out_b$node_wait)
  expect_identical(out_a$node_active_service, out_b$node_active_service)
  expect_identical(out_a$total_time_in_system, out_b$total_time_in_system)
})


# ---------------------------------------------------------------------------
# Works with template 2 (larger network)
# ---------------------------------------------------------------------------

test_that("runs without error on template 2 (16-node stroke pathway)", {
  inp <- make_inputs("input_template_2.csv", "cal_input_2.csv")

  set.seed(42)
  expect_no_error(
    suppressWarnings(
      run_single_replication(
        j = 1,
        var_input = inp$var_input, syst_names = inp$syst_names,
        syst_names_single = inp$syst_names_single, nodes = inp$nodes,
        delay_dist = inp$delay_dist, delay_param = inp$delay_param,
        delay_list = inp$delay_list, cap_cal_input = inp$cap_cal_input,
        arr_cal_input = inp$arr_cal_input, record_scale = inp$record_scale,
        na_lim = inp$na_lim, rpi = inp$rpi, warm_up = inp$warm_up,
        sim_time = inp$sim_time, t.period = inp$t.period,
        node_names = inp$node_names, reps = inp$reps
      )
    )
  )
})
