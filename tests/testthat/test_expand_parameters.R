context("expand_parameters")

test_that("a parameter list is expanded", {
  param <- list(rates = 1, distances = 1)
  expanded <- expand_parameters(param)

  expect_length(expanded$rates, 3)
  expect_length(expanded$distances, 3)
  expect_equal(attr(expanded, "expanded"), TRUE)
})

test_that("a parameter list can't be expanded more than once", {
  param <- list(rates = 1, distances = 1)
  expanded <- expand_parameters(param)
  expanded_again <- expand_parameters(param)

  expect_length(expanded_again$rates, 3)
  expect_length(expanded_again$distances, 3)
  expect_equal(attr(expanded, "expanded"), TRUE)
})
