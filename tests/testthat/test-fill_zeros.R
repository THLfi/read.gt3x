testthat::test_that("fill_zeros replaces runs of all-zero rows", {
  df <- data.frame(
    X = c(0, 1, 0, 0, 2, 0),
    Y = c(0, 3, 0, 0, 4, 0),
    Z = c(0, 5, 0, 0, 6, 0)
  )

  out <- fill_zeros(df)

  testthat::expect_equal(out$X, c(0, 1, 1, 1, 2, 2))
  testthat::expect_equal(out$Y, c(0, 3, 3, 3, 4, 4))
  testthat::expect_equal(out$Z, c(0, 5, 5, 5, 6, 6))
})

testthat::test_that("fill_zeroes is an alias for fill_zeros", {
  df <- data.frame(
    X = c(1, 0),
    Y = c(2, 0),
    Z = c(3, 0)
  )

  testthat::expect_equal(fill_zeroes(df), fill_zeros(df))
})

testthat::test_that("fill_zeros requires XYZ columns", {
  df <- data.frame(X = 1, Y = 2)

  testthat::expect_error(
    fill_zeros(df),
    "must contain columns X, Y, Z"
  )
})
