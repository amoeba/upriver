context("timings")

source("testthat_helper.R")

test_that("correct timings are calculated", {
  expect_equal(timings(500, arrival_simple, params_simple), 11:20)
})

test_that("adding an offset results in the correct timing calculation", {
  expect_equal(timings(200, arrival_single, params_simple, start_position = 100), 7)
})
