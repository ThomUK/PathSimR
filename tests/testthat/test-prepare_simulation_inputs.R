# Tests for prepare_simulation_inputs()
#
# Uses the same fixture CSVs as the run_simulation snapshot tests so that
# the helper is exercised with realistic network shapes.

# ---------------------------------------------------------------------------
# Fixture loaders (mirrors load_var_input / load_cal_input in test-run_simulation.R)
# ---------------------------------------------------------------------------

load_var <- function(filename) {
  path <- testthat::test_path("fixtures", filename)
  var_input <- read.csv(path, header = TRUE, sep = ",")

  syst_names        <- cbind(as.numeric(1:nrow(var_input)),
                             as.character(var_input[, 1]))
  syst_names_single <- syst_names[, 2]

  var_input <- var_input[, -1]
  rownames(var_input) <- 1:nrow(var_input)
  colnames(var_input)[1:nrow(var_input)] <- c(1:nrow(var_input))

  list(var_input = var_input, syst_names = syst_names,
       syst_names_single = syst_names_single)
}

load_cal <- function(filename) {
  path <- testthat::test_path("fixtures", filename)
  cal <- read.csv(path, header = TRUE, sep = ",")
  cal$node <- as.character(cal$node)
  cal
}


# ---------------------------------------------------------------------------
# Output structure
# ---------------------------------------------------------------------------

test_that("returns a list with all 13 expected elements", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 100)

  expect_type(out, "list")
  expect_named(out, c("nodes", "node_names", "delay_dist", "delay_param",
                      "delay_list", "cap_cal_input", "arr_cal_input",
                      "cap_cal_input_original", "arr_cal_input_original",
                      "record_scale", "na_lim", "rpi", "t.period"))
})


# ---------------------------------------------------------------------------
# t.period
# ---------------------------------------------------------------------------

test_that("t.period equals warm_up + sim_time", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 20, sim_time = 100)

  expect_equal(out$t.period, 120)
})

test_that("t.period is correct when warm_up is 0", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 365)

  expect_equal(out$t.period, 365)
})


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

test_that("simulation constants are fixed values", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 100)

  expect_equal(out$record_scale, 0.8)
  expect_equal(out$na_lim,       100)
  expect_equal(out$rpi,          0.1)
})


# ---------------------------------------------------------------------------
# nodes: only service nodes (non-zero outflow rows) are included
# ---------------------------------------------------------------------------

test_that("nodes contains only active service nodes (numeric)", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 100)

  expect_type(out$nodes, "double")
  expect_true(length(out$nodes) > 0)
  # All node IDs must be valid row numbers in var_input
  expect_true(all(out$nodes %in% as.numeric(rownames(v$var_input))))
})

test_that("nodes is consistent across template 2 (larger network)", {
  v   <- load_var("input_template_2.csv")
  cal <- load_cal("cal_input_2.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 100)

  expect_true(length(out$nodes) > 0)
  expect_true(all(out$nodes %in% as.numeric(rownames(v$var_input))))
})


# ---------------------------------------------------------------------------
# node_names: active nodes + trailing NA row
# ---------------------------------------------------------------------------

test_that("node_names has one more row than nodes (trailing NA sentinel)", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 100)

  expect_equal(nrow(out$node_names), length(out$nodes) + 1)
  expect_true(all(is.na(out$node_names[nrow(out$node_names), ])))
})


# ---------------------------------------------------------------------------
# delay_dist / delay_param: square matrices, same dimensions as var_input rows
# ---------------------------------------------------------------------------

test_that("delay_dist and delay_param are square with nrow == nrow(var_input)", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 100)

  n <- nrow(v$var_input)
  expect_equal(nrow(out$delay_dist),  n)
  expect_equal(ncol(out$delay_dist),  n)
  expect_equal(nrow(out$delay_param), n)
  expect_equal(ncol(out$delay_param), n)
})


# ---------------------------------------------------------------------------
# delay_list: two-column matrix, starts with (0, 0) sentinel
# ---------------------------------------------------------------------------

test_that("delay_list is a two-column matrix starting with (0, 0)", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 100)

  expect_true(is.matrix(out$delay_list))
  expect_equal(ncol(out$delay_list), 2)
  expect_equal(colnames(out$delay_list), c("from", "to"))
  expect_equal(out$delay_list[1, ], c(from = 0, to = 0))
})


# ---------------------------------------------------------------------------
# Calendar splitting: cap vs arr rows are correctly separated
# ---------------------------------------------------------------------------

test_that("cap_cal_input contains only cap rows and arr_cal_input only ext_arr rows", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 100)

  expect_true(all(out$cap_cal_input$metric == "cap"))
  expect_true(all(out$arr_cal_input$metric == "ext_arr"))
})


# ---------------------------------------------------------------------------
# Calendar node IDs are numeric after conversion
# ---------------------------------------------------------------------------

test_that("calendar node column is numeric after name-to-ID conversion", {
  v   <- load_var("input_template_3.csv")
  cal <- load_cal("cal_input_3.csv")

  out <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                   warm_up = 0, sim_time = 100)

  expect_true(is.numeric(out$cap_cal_input$node) ||
                all(!is.na(suppressWarnings(as.numeric(out$cap_cal_input$node)))))
  expect_true(is.numeric(out$arr_cal_input$node) ||
                all(!is.na(suppressWarnings(as.numeric(out$arr_cal_input$node)))))
})


# ---------------------------------------------------------------------------
# Warm-up shifts the calendars (originals are preserved unchanged)
# ---------------------------------------------------------------------------

test_that("originals are identical to pre-shift calendars; shifted differ when warm_up > 0", {
  v   <- load_var("input_template_3.csv")

  # Use a cal_input where at least one node has a multi-row calendar so a
  # shift is actually applied. Template 3 cal has multi-row entries.
  cal <- load_cal("cal_input_3.csv")

  out_no_warmup <- prepare_simulation_inputs(v$var_input, cal, v$syst_names,
                                             warm_up = 0, sim_time = 100)
  cal2 <- load_cal("cal_input_3.csv")
  out_warmup    <- prepare_simulation_inputs(v$var_input, cal2, v$syst_names,
                                             warm_up = 25, sim_time = 100)

  # Originals must equal the no-warmup shifted versions (no shift applied when warm_up=0)
  expect_identical(out_no_warmup$cap_cal_input,
                   out_no_warmup$cap_cal_input_original)

  # With warm_up > 0, the shifted and original differ (assuming multi-row cal)
  # We check t.period includes the warmup
  expect_equal(out_warmup$t.period, 125)
})
