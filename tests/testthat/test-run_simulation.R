# Snapshot tests for run_simulation()
#
# These tests capture the full output of the simulation engine for two
# reference networks. On first run they create golden-master snapshots in
# tests/testthat/_snaps/. All subsequent runs (including after refactoring)
# diff against those snapshots. Any change to simulation outputs will cause a
# test failure, making it immediately visible.
#
# To regenerate snapshots after an intentional behaviour change run:
#   testthat::snapshot_accept("run_simulation")

# ---------------------------------------------------------------------------
# Helpers: thin wrappers around the exported reader functions
# ---------------------------------------------------------------------------

load_var_input <- read_network_template

load_cal_input <- read_calendar_template

# Helper: remove non-deterministic elements before snapshotting.
# - ggplot objects contain environments and are not reliably serialisable.
# - ptm is a proc.time() snapshot that differs on every run.
# The data underlying all plots IS captured in the other list elements.
strip_non_deterministic <- function(x) {
  is_plot <- sapply(x, inherits, what = "gg")
  is_timer <- names(x) == "ptm"
  x[!(is_plot | is_timer)]
}


# ---------------------------------------------------------------------------
# Template 2: Stroke pathway (16 nodes, complex topology, exp distributions)
# ---------------------------------------------------------------------------
test_that("run_simulation snapshot matches for template 2 (stroke)", {
  skip_on_cran()

  v <- load_var_input(testthat::test_path("fixtures", "input_template_2.csv"))
  cal_input <- load_cal_input(testthat::test_path("fixtures", "cal_input_2.csv"))

  set.seed(42)
  result <- run_simulation(
    var_input         = v$var_input,
    cal_input         = cal_input,
    sim_time          = 100,
    warm_up           = 0,
    reps              = 5,
    syst_names        = v$syst_names,
    syst_names_single = v$syst_names_single,
    time_unit         = "days",
    session           = NULL
  )

  expect_equal(length(result), 73)
  expect_snapshot_value(strip_non_deterministic(result), style = "serialize")
})


# ---------------------------------------------------------------------------
# Template 3: Simple 4-node network (uniform distributions, transition delays)
# ---------------------------------------------------------------------------
test_that("run_simulation snapshot matches for template 3 (simple 4-node)", {
  skip_on_cran()

  v <- load_var_input(testthat::test_path("fixtures", "input_template_3.csv"))
  cal_input <- load_cal_input(testthat::test_path("fixtures", "cal_input_3.csv"))

  set.seed(42)
  result <- run_simulation(
    var_input         = v$var_input,
    cal_input         = cal_input,
    sim_time          = 100,
    warm_up           = 0,
    reps              = 5,
    syst_names        = v$syst_names,
    syst_names_single = v$syst_names_single,
    time_unit         = "days",
    session           = NULL
  )

  expect_equal(length(result), 73)
  expect_snapshot_value(strip_non_deterministic(result), style = "serialize")
})


# ---------------------------------------------------------------------------
# Cluster cleanup: stopCluster must be called even when the simulation errors
# after makeCluster — prevents orphaned workers holding ports on subsequent runs
# ---------------------------------------------------------------------------
test_that("stopCluster is called via on.exit when simulation errors after makeCluster", {
  stop_called <- FALSE

  local_mocked_bindings(
    makeCluster = function(...) list(), # fake cluster, no workers
    clusterExport = function(...) stop("forced test error"), # fail before any sim runs
    stopCluster = function(cl) stop_called <<- TRUE,
    .package = "parallel"
  )

  v <- load_var_input(test_path("fixtures", "input_template_3.csv"))
  cal_input <- load_cal_input(test_path("fixtures", "cal_input_3.csv"))

  expect_error(
    run_simulation(
      var_input         = v$var_input,
      cal_input         = cal_input,
      sim_time          = 10,
      warm_up           = 0,
      reps              = 1,
      syst_names        = v$syst_names,
      syst_names_single = v$syst_names_single
    ),
    "forced test error"
  )

  expect_true(stop_called, "stopCluster must be called even when simulation errors mid-run")
})


# ---------------------------------------------------------------------------
# Template 3 with warm-up: verifies calendar-shifting logic is preserved
# ---------------------------------------------------------------------------
test_that("run_simulation snapshot matches for template 3 with warm-up", {
  skip_on_cran()

  v <- load_var_input(testthat::test_path("fixtures", "input_template_3.csv"))
  cal_input <- load_cal_input(testthat::test_path("fixtures", "cal_input_3.csv"))

  set.seed(42)
  result <- run_simulation(
    var_input         = v$var_input,
    cal_input         = cal_input,
    sim_time          = 100,
    warm_up           = 20,
    reps              = 5,
    syst_names        = v$syst_names,
    syst_names_single = v$syst_names_single,
    time_unit         = "days",
    session           = NULL
  )

  expect_equal(length(result), 73)
  expect_snapshot_value(strip_non_deterministic(result), style = "serialize")
})
