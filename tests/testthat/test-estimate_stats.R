test_that("small samples use formula (5) for the mean and formula (16) for the SD", {
  res <- estimate_stats(median = 10, minimum = 4, maximum = 20, sample_size = 12)
  expect_s3_class(res, "data.frame")
  expect_equal(res$mean, 11)
  expect_equal(res$sd, sqrt(65 / 3))
  expect_equal(res$variance, 65 / 3)
  expect_identical(res$mean_method, "Hozo formula (5)")
  expect_identical(res$sd_method, "Hozo formula (16)")
})

test_that("mean_formula = '4' adds the sample-size correction", {
  res <- estimate_stats(10, 4, 20, 12, mean_formula = "4")
  expect_equal(res$mean, 11 + 1 / 12)
  expect_identical(res$mean_method, "Hozo formula (4)")
  expect_equal(estimate_stats(10, 4, 20, 12, mean_formula = 4), res)
  expect_error(estimate_stats(10, 4, 20, 12, mean_formula = "6"))
})

test_that("moderate samples use range / 4", {
  res <- estimate_stats(10, 4, 20, 20)
  expect_equal(res$mean, 11)
  expect_equal(res$sd, 4)

  res <- estimate_stats(median = 10, minimum = 5, maximum = 15, sample_size = 30)
  expect_equal(res$mean, 10)
  expect_equal(res$sd, 2.5)
  expect_equal(res$variance, 6.25)
  expect_identical(res$mean_method, "median")
  expect_identical(res$sd_method, "range/4")
})

test_that("large samples use the median and range / 6", {
  res <- estimate_stats(100, 90, 110, 100)
  expect_equal(res$mean, 100)
  expect_equal(res$sd, 20 / 6)
  expect_identical(res$sd_method, "range/6")
})

test_that("the estimator changes at n = 15, 25 and 70", {
  res <- estimate_stats(10, 4, 20, c(15, 16, 25, 26, 70, 71))
  expect_identical(
    res$sd_method,
    c("Hozo formula (16)", "range/4", "range/4", "range/4", "range/4", "range/6")
  )
  expect_identical(res$mean_method,
                   c(rep("Hozo formula (5)", 3), rep("median", 3)))
})

test_that("inputs are vectorised and length-1 inputs are recycled", {
  res <- estimate_stats(c(10, 30), c(4, 18), c(20, 52), 40)
  expect_equal(nrow(res), 2L)
  single <- estimate_stats(30, 18, 52, 40)
  expect_equal(res$mean[2], single$mean)
  expect_equal(res$sd[2], single$sd)
})

test_that("missing values give NA in the matching row only", {
  res <- estimate_stats(c(10, NA), c(4, 18), c(20, 52), c(12, 40))
  expect_equal(res$mean[1], 11)
  expect_true(is.na(res$mean[2]))
  expect_true(is.na(res$sd[2]))
  expect_true(is.na(res$sd_method[2]))

  all_missing <- estimate_stats(NA, NA, NA, NA)
  expect_true(is.na(all_missing$mean))
  expect_type(all_missing$mean, "double")
})

test_that("invalid inputs give clear errors", {
  expect_error(estimate_stats(10, 15, 20, 30), "`minimum` is greater than `median`")
  expect_error(estimate_stats(20, 10, 15, 30), "`median` is greater than `maximum`")
  expect_error(estimate_stats(10, 5, 15, 1), "whole number of at least 2")
  expect_error(estimate_stats(10, 5, 15, 10.5), "whole number of at least 2")
  expect_error(estimate_stats("10", 5, 15, 30), "`median` must be numeric")
  expect_error(estimate_stats(10, 5, Inf, 30), "finite")
  expect_error(estimate_stats(c(10, 11), c(5, 6, 7), 15, 30), "same length")
  expect_error(estimate_stats(numeric(0), 5, 15, 30), "at least one value")
  expect_error(estimate_stats(c(10, 10), c(5, 15), c(15, 20), 30), "row 2")
})

test_that("warnings flag very small samples and zero ranges", {
  expect_warning(estimate_stats(50, 40, 60, 5), "below 8")
  expect_warning(res <- estimate_stats(20, 20, 20, 10), "standard deviation is 0")
  expect_equal(res$mean, 20)
  expect_equal(res$sd, 0)
})

test_that("typical inputs give no warning", {
  expect_no_warning(estimate_stats(10, 5, 15, 30))
})
