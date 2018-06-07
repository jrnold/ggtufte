context("range_frame")

test_that("range_breaks works", {
  f <- range_breaks(5)
  expect_is(f, "function")
  x <- 1:20
  out <- f(x)
  expect_equal(min(out), min(x))
  expect_equal(max(out), max(x))
})