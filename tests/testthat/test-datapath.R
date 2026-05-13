testthat::test_that("Getting datapath", {
  datapath <- tryCatch(gt3x_datapath(), error = function(e) NULL)
  if (is.null(datapath)) {
    testthat::skip("Sample data unavailable without network access")
  }
  gt3x_datapath(1)
  testthat::expect_error(gt3x_filename(0))
  testthat::expect_error(gt3x_filename(100))
  fnames =     c("EE_left_29.5.2017-05-30.gt3x",
                 "SS_left_19.5.2017-05-22.gt3x")
  testthat::expect_equal(
    gt3x_filename(zipped = TRUE),
    paste0(fnames, ".zip")
  )
  testthat::expect_equal(basename(list_gt3x(gt3x_datapath())), fnames)

  testthat::expect_null(read.gt3x:::unzip_zipped_gt3x(NULL))
})
