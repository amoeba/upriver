context("positions")

source("testthat_helper.R")

test_that("a fish with zero days of travel is at position zero", {
    expect_equal(positions(0, params_simple), 0)
})

test_that("a fish with one day of travel is at position 50", {
  expect_equal(positions(1, params_simple), 1 * params_simple$rates[2])
})

test_that("a fish travels upriver without making a mistake", {
  expect_equal(positions(0:30, params_simple), 0:30 * 50)
  expect_equal(positions(0:30, params_complex), c(0, 43.5, 87, 133.938620689655, 182.838620689655, 231.738620689655,
                                                  280.638620689655, 325.71079895635, 361.91079895635, 398.272669648635,
                                                  442.178260981988, 492.178260981988, 542.178260981988, 592.178260981988,
                                                  642.178260981988, 692.178260981988, 742.178260981988, 792.178260981988,
                                                  842.178260981988, 892.178260981988, 942.178260981988, 992.178260981988,
                                                  1042.17826098199, 1092.17826098199, 1142.17826098199, 1192.17826098199,
                                                  1242.17826098199, 1292.17826098199, 1342.17826098199, 1392.17826098199,
                                                  1442.17826098199))
})

test_that("a fish can travel a really far distance", {
  expect_gt(positions(1000, params_simple), 2000)
})

test_that("a fish can travel a fraction of a day", {
  expect_equal(positions(c(0.5, 1.5, 2.5, 3.5), params_simple), c(25, 75, 125, 175))
})

test_that("a fish travels even when the path is complex", {
  expect_lt(positions(5, params_complex) - 231.7386, 1e-04)
})

test_that("position for multiple fish can be calculated", {
  expect_equal(positions(1:5, params_simple), c(50, 100, 150, 200, 250))
})

test_that("negative days returns NA", {
  expect_equal(positions(-5, params_simple), NA)
})

test_that("negative and non-negative days returns NAs and positions", {
  expect_equal(positions(seq(-5, 5), params_simple), c(rep(NA, 5), seq(0, 250, 50)))
})
