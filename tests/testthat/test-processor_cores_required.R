test_that("it does not assign more cores than required sim replications", {

  replications <- 5
  cores <- 8

  n <- processor_cores_required(replications, cores)

  expect_equal(n, 5)

})

test_that("it assignes at least one core", {

  replications <- 10
  cores <- 1

  n <- processor_cores_required(replications, cores)

  expect_equal(n, 1)

})
