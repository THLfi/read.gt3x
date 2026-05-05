testthat::context("Reading in data without rounding")

gt3xfile <-
  system.file(
    "extdata", "TAS1H30182785_2019-09-17.gt3x",
    package = "read.gt3x")



testthat::test_that("has_log_info", {
  gt3xdataZeroes <- read.gt3x(gt3xfile,
                              imputeZeroes = TRUE,
                              asDataFrame = TRUE,
                              digits = 5)

  testthat::expect_equal(
    gt3xdataZeroes$X*100000, round(gt3xdataZeroes$X*100000)
  )

  testthat::expect_error({
    testthat::expect_equal(
      gt3xdataZeroes$X*1000, round(gt3xdataZeroes$X*1000)
    )
  })

  gt3xdataZeroes <- read.gt3x(gt3xfile,
                              imputeZeroes = TRUE,
                              asDataFrame = TRUE,
                              digits = 3)

  testthat::expect_equal(
    gt3xdataZeroes$X*1000, round(gt3xdataZeroes$X*1000)
  )


})
