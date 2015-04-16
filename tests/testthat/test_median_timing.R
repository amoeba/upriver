context("median_timing")

source("testthat_helper.R")

test_that("median timing is calculated correctly", {
  expect_equal(median_timing(0, arrival_simple, params_simple), 5)
  expect_equal(median_timing(50, arrival_simple, params_simple), 6)
  expect_equal(median_timing(250, arrival_simple, params_simple), 10)
  expect_equal(median_timing(500, arrival_simple, params_simple), 15)
})

test_that("median timing is calculated correctly even when not on boundaries", {
  expect_equal(median_timing(25, arrival_simple, params_simple), 6)
  expect_equal(median_timing(75, arrival_simple, params_simple), 7)
})
