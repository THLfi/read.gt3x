path <- system.file(
  "extdata", "TAS1H30182785_2019-09-17.gt3x",
  package = "read.gt3x")

testthat::context("Need to get rid of certain fields for coverage")

testthat::test_that("Trying without Acceleration Scale", {
  testthat::skip_if_not_installed("zip")
  run_file = destroy_field(path)
  result = parse_gt3x_info(run_file)
  testthat::expect_equal(result$`Acceleration Scale`, 256L)
  testthat::expect_true(get_n_samples(result) == 240500L)
  run_file = destroy_field(path, field = c("Last Sample Time", "Download Date", "Sample Rate"))
  result = parse_gt3x_info(run_file)
  testthat::expect_warning({
    out = get_n_samples(result)
  })
  testthat::expect_true(attr(out, "bad"))
  attr(out, "bad") = NULL
  testthat::expect_equal(out, 8.64e+08)


  run_file = destroy_field(path,
                           field = c("Last Sample Time"),
                           replace_field = list("Stop Date" = "0",
                                                "Sample Rate" = "Hey"))
  testthat::expect_warning({
    result = parse_gt3x_info(run_file)
  })
  testthat::expect_warning({
    out = get_n_samples(result)
  })

  run_file = destroy_field(path, field = "all")

  testthat::expect_error({
    result = unzip.gt3x(run_file, verbose = TRUE)
  })

  run_file = destroy_field(path,
                           replace_field = list("Serial Number" = ""))
  testthat::expect_warning({
    result = parse_gt3x_info(run_file)
  })

})
