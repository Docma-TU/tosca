context("precisionrecall")

test_that("precisionrecall", {

  suppressWarnings(RNGversion("3.5.0"))
  set.seed(24601)

  w <- c(0.5, 0.1, 0.2, 0.2)
  p <- c(0.01, 0.8, 0.75, 0.95)
  subset <- c(FALSE, TRUE, FALSE, TRUE)
  n <- c(40, 20, 15, 33)
  expect_equal(0.9, precision(w, p, subset))
  expect_equal(0.0001375758, round(vprecision(w, p, subset, n), 10))
  expect_equal(0.6352941, round(recall(w, p, subset), 7))
  expect_equal(0.001356794, round(vrecall(w, p, subset, n), 9))
  
  expect_error(precision(w[-1], p, subset))
  expect_error(precision(w, p[-1], subset))
  expect_error(precision(w, p, subset[-1]))

  expect_error(vprecision(w[-1], p, subset, n))
  expect_error(vprecision(w, p[-1], subset, n))
  expect_error(vprecision(w, p, subset[-1], n))
  expect_error(vprecision(w, p, subset, n[-1]))

  expect_error(recall(w[-1], p, subset))
  expect_error(recall(w, p[-1], subset))
  expect_error(recall(w, p, subset[-1]))

  expect_error(vrecall(w[-1], p, subset, n))
  expect_error(vrecall(w, p[-1], subset, n))
  expect_error(vrecall(w, p, subset[-1], n))
  expect_error(vrecall(w, p, subset, n[-1]))
  
})
