
url <- paste0("https://ndownloader.figshare.com/files/25557302")
path <- tempfile(fileext = ".gt3x.gz")
dl <- utils::download.file(url, destfile = path, mode = "wb")

testthat::test_that("Reading in Old MRA", {
  testthat::expect_error({
    res <- read.gt3x::read.gt3x(path = character(0))
  })

  !have_log_and_info(path)
  testthat::expect_warning({
    res <- read.gt3x::read.gt3x(path, verbose = 2, debug = TRUE, cleanup = TRUE)
  }, regexp = NA)
  testthat::expect_is(res, "activity")
  testthat::expect_is(res, "matrix")
  testthat::expect_equal(colnames(res), c("X", "Y", "Z"))
  print(res)
  head(res)

  cm <- unname(apply(res, 2, mean))
  testthat::expect_equal(
    cm,
    c(-0.228435613712367, 0.0223566340380155, -0.429840057237743),
    tolerance = 1e-5)
  testthat::expect_equal(unname(res[4823, "Y"]), -0.012)

  all_attr <- attributes(res)
  testthat::expect_true(all_attr$old_version)
  testthat::expect_equal(all_attr$sample_rate, 30)
  rm(all_attr)
  res = as.data.frame(res, verbose = TRUE)
  print(res)
  head(res)
  rm(res)
})


testthat::test_that("Reading in Old MRA LUX", {
  testthat::expect_warning({
    res <- read.gt3x::read.gt3x(path, verbose = 2,
                                debug = TRUE, add_light = TRUE)
  }, regexp = NA)
  testthat::expect_is(res, "activity")
  testthat::expect_is(res, "matrix")
  testthat::expect_equal(colnames(res), c("X", "Y", "Z"))

  res = as.data.frame(res, verbose = TRUE)
  testthat::expect_equal(colnames(res), c("time", "X", "Y", "Z", "lux"))

  testthat::skip_if_not_installed("zip")
  run_file = destroy_field(path)
  result = parse_gt3x_info(run_file)
  testthat::expect_warning({
    res <- read.gt3x::read.gt3x(path, verbose = 2,
                                debug = TRUE, add_light = TRUE)
  }, regexp = NA)

})


