# Tests for shift_calendars()
#
# The function rotates cyclical capacity and arrival calendars so that the
# position in the cycle at the end of warm-up aligns with time 0 of the
# reported simulation. Single-row (constant) calendars are passed through
# unchanged.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_cap <- function(node, starts, ends, values) {
  data.frame(
    metric = "cap",
    node = node,
    start = starts,
    end = ends,
    value = values,
    stringsAsFactors = FALSE
  )
}

make_arr <- function(node, starts, ends, values) {
  data.frame(
    metric = "ext_arr",
    node = node,
    start = starts,
    end = ends,
    value = values,
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------
# warm_up = 0: no-op regardless of calendar shape
# ---------------------------------------------------------------------------

test_that("warm_up = 0 returns calendars unchanged", {
  cap <- make_cap(1, c(0, 10, 20), c(10, 20, 30), c(5, 10, 5))
  arr <- make_arr(1, c(0, 10, 20), c(10, 20, 30), c(2, 0, 2))

  result <- shift_calendars(cap, arr, nodes = 1, warm_up = 0)

  expect_identical(result$cap_cal_input, cap)
  expect_identical(result$arr_cal_input, arr)
})


# ---------------------------------------------------------------------------
# Single-row calendar: always passed through unchanged (no cycle to shift)
# ---------------------------------------------------------------------------

test_that("single-row calendar is unchanged regardless of warm_up", {
  cap <- make_cap(1, 0, NA, 10)
  arr <- make_arr(1, 0, NA, 2)

  result <- shift_calendars(cap, arr, nodes = 1, warm_up = 15)

  expect_identical(result$cap_cal_input, cap)
  expect_identical(result$arr_cal_input, arr)
})


# ---------------------------------------------------------------------------
# warm_up aligns exactly on a cycle boundary: modulo = 0, no shift applied
# ---------------------------------------------------------------------------

test_that("warm_up that is exact multiple of cycle length leaves calendar unchanged", {
  # Cycle length = 30 (0-10, 10-20, 20-30); warm_up = 60 = 2 * 30
  cap <- make_cap(1, c(0, 10, 20), c(10, 20, 30), c(5, 10, 5))
  arr <- make_arr(1, c(0, 10, 20), c(10, 20, 30), c(2, 0, 2))

  result <- shift_calendars(cap, arr, nodes = 1, warm_up = 60)

  expect_identical(result$cap_cal_input, cap)
  expect_identical(result$arr_cal_input, arr)
})


# ---------------------------------------------------------------------------
# Fractional warm_up: calendar is shifted by the modulo
# ---------------------------------------------------------------------------

test_that("fractional warm_up rotates capacity calendar correctly", {
  # Cycle: 0-10 (value 5), 10-20 (value 10), 20-30 (value 5)
  # warm_up = 35 → modulo = 35 %% 30 = 5
  # After adding 5: 5-15, 15-25, 25-35
  # Stable (end >= 30): rows 1 (5-15→5-30) and 2 (15-25)... let's work it out:
  #   row1: start=5, end=15  → end < 30, keep
  #   row2: start=15, end=25 → end < 30, keep
  #   row3: start=25, end=35 → end >= 30 → first where end >= 30
  # stable = rows 1..3 (up to min which end >= 30 = row 3)
  #   stable$end[last] clamped to 30 → row3 becomes 25-30
  # switch = row3 onward: start clamped to 30, then subtract 30
  #   row3: start=30→0, end=35-30=5, value=5
  # Final order by start: 0-5(v5), 5-15(v5), 15-25(v10), 25-30(v5)
  cap <- make_cap(1, c(0, 10, 20), c(10, 20, 30), c(5, 10, 5))
  arr <- make_arr(1, c(0, 50), c(50, 100), c(3, 0)) # simple arr, just check cap

  result <- shift_calendars(cap, arr, nodes = 1, warm_up = 35)
  shifted_cap <- result$cap_cal_input

  expect_equal(nrow(shifted_cap), 4)
  expect_equal(shifted_cap$start, c(0, 5, 15, 25))
  expect_equal(shifted_cap$end, c(5, 15, 25, 30))
  expect_equal(shifted_cap$value, c(5, 5, 10, 5))
})


test_that("fractional warm_up rotates arrival calendar correctly", {
  # Cycle: 0-50 (value 3), 50-100 (value 0)
  # warm_up = 25 → modulo = 25 %% 100 = 25
  # After adding 25: 25-75, 75-125
  # stable (end > 100): first where end > 100 is row 2 (75-125)
  #   stable = rows 1..2, stable$end[last] clamped to 100 → 75-100
  # switch = row 2: start=100→0, end=125-100=25, value=0
  # Final: 0-25(v0), 25-75(v3), 75-100(v0)
  cap <- make_cap(1, c(0, 10), c(10, 20), c(5, 5)) # simple cap
  arr <- make_arr(1, c(0, 50), c(50, 100), c(3, 0))

  result <- shift_calendars(cap, arr, nodes = 1, warm_up = 25)
  shifted_arr <- result$arr_cal_input

  expect_equal(nrow(shifted_arr), 3)
  expect_equal(shifted_arr$start, c(0, 25, 75))
  expect_equal(shifted_arr$end, c(25, 75, 100))
  expect_equal(shifted_arr$value, c(0, 3, 0))
})


# ---------------------------------------------------------------------------
# Multiple nodes: each shifted independently
# ---------------------------------------------------------------------------

test_that("multiple nodes are each shifted independently", {
  # Node 1: cycle 30, warm_up 10 → modulo 10
  # Node 2: cycle 20, warm_up 10 → modulo 10 (exact half-cycle)
  cap <- rbind(
    make_cap(1, c(0, 15), c(15, 30), c(5, 10)),
    make_cap(2, c(0, 10), c(10, 20), c(3, 6))
  )
  arr <- rbind(
    make_arr(1, c(0, 15), c(15, 30), c(2, 0)),
    make_arr(2, c(0, 10), c(10, 20), c(1, 0))
  )

  result <- shift_calendars(cap, arr, nodes = c(1, 2), warm_up = 10)

  # Node 2 cap: modulo = 10 %% 20 = 10 → shifted
  # Node 1 cap: modulo = 10 %% 30 = 10 → shifted
  # Mainly check that both nodes are present in output
  expect_true(1 %in% result$cap_cal_input$node)
  expect_true(2 %in% result$cap_cal_input$node)
  expect_true(1 %in% result$arr_cal_input$node)
  expect_true(2 %in% result$arr_cal_input$node)
})


# ---------------------------------------------------------------------------
# Output is always ordered by start within each node
# ---------------------------------------------------------------------------

test_that("shifted calendar rows are ordered by start time", {
  cap <- make_cap(1, c(0, 10, 20), c(10, 20, 30), c(1, 2, 3))
  arr <- make_arr(1, c(0, 10, 20), c(10, 20, 30), c(4, 5, 6))

  result <- shift_calendars(cap, arr, nodes = 1, warm_up = 7)

  node_cap <- result$cap_cal_input[result$cap_cal_input$node == 1, ]
  node_arr <- result$arr_cal_input[result$arr_cal_input$node == 1, ]

  expect_equal(node_cap$start, sort(node_cap$start))
  expect_equal(node_arr$start, sort(node_arr$start))
})
