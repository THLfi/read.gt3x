url <- paste0("https://github.com/THLfi/read.gt3x/",
              "files/", "3522749/",
              "GT3X%2B.01.day.gt3x.zip")
destfile <- tempfile(fileext = ".zip")
dl <- utils::download.file(url, destfile = destfile, mode = "wb")
gt3x_file <- utils::unzip(destfile, exdir = tempdir())
gt3x_file <- gt3x_file[!grepl("__MACOSX", gt3x_file)]
path <- gt3x_file

gz <- R.utils::gzip(path, remove = FALSE, overwrite = TRUE)
gz_file <- tempfile(fileext = ".gt3x.gz")
file.copy(gz, gz_file)

testthat::test_that("Reading in Old format works", {
  have_log_and_info(path)
  testthat::expect_warning({
    res <- read.gt3x::read.gt3x(path, verbose = 2, debug = TRUE)
  }, regexp = NA)
  testthat::expect_is(res, "activity")
  testthat::expect_is(res, "matrix")
  testthat::expect_equal(colnames(res), c("X", "Y", "Z"))
  print(res)
  head(res)

  cm <- unname(apply(res, 2, mean))
  testthat::expect_equal(cm, c(
    -0.228402625555557,
    0.447592941851854,
    0.11958707074074
  ), tolerance = 1e-5)
  testthat::expect_equal(unname(res[4823, "Y"]), 0.528)

  all_attr <- attributes(res)
  testthat::expect_true(all_attr$old_version)
  testthat::expect_equal(all_attr$sample_rate, 30)
  rm(all_attr)
  res = as.data.frame(res, verbose = TRUE)
  print(res)
  head(res)
  rm(res)
})


testthat::test_that("Converting Old to Data.frame", {
  res <- read.gt3x::read.gt3x(path, asDataFrame = TRUE)
  testthat::expect_is(res, "activity_df")
  testthat::expect_is(res, "data.frame")
  testthat::expect_equal(colnames(res), c("time", "X", "Y", "Z"))

  cm <- unname(apply(res[, c("X", "Y", "Z")], 2, mean))
  testthat::expect_equal(cm, c(
    -0.228402625555557,
    0.447592941851854,
    0.11958707074074
  ), tolerance = 1e-5)
  testthat::expect_equal(
    res$time[48], structure(1340794441.56667,
                            class = c("POSIXct", "POSIXt"), tzone = "GMT"
    )
  )
  testthat::expect_equal(res$Y[4823], 0.528)

  all_attr <- attributes(res)
  rm(res)
  testthat::expect_true(all_attr$old_version)
  testthat::expect_equal(all_attr$sample_rate, 30)
  rm(all_attr)
})

testthat::test_that("Converting Old with .gz file", {
  res <- read.gt3x::read.gt3x(path, asDataFrame = FALSE)
  df2 <- read.gt3x(gz_file,
                   asDataFrame = FALSE, verbose = 2,
                   cleanup = TRUE
  )
  testthat::expect_equal(res, df2)
  rm(res)
  rm(df2)
})


tzHel = "Europe/Helsinki"
tzLon = "Europe/London"

testthat::test_that("read.gt3x old format - timezone configuration == timezone worn (Europe/London)", {
  gt3xdata_tz_equal <- read.gt3x(path, asDataFrame = TRUE, imputeZeroes = FALSE,
                                 desiredtz = tzLon, configtz = tzLon)
  testthat::expect_true(inherits(gt3xdata_tz_equal$time[1], "POSIXct"))
  testthat::expect_equal(gt3xdata_tz_equal$time[1], as.POSIXct("2012-06-27 10:54:00", tz = tzLon))
})

testthat::test_that("read.gt3x old format - timezone configuration (Europe/Helsinki) > timezone worn (Europe/London)", {
  gt3xdata_confidHel_wornLon <- read.gt3x(path, asDataFrame = TRUE, imputeZeroes = FALSE,
                                         desiredtz = tzLon, configtz = tzHel)
  testthat::expect_true(inherits(gt3xdata_confidHel_wornLon$time[1], "POSIXct"))
  testthat::expect_equal(gt3xdata_confidHel_wornLon$time[1], as.POSIXct("2012-06-27 08:54:00", tz = tzLon))
})


testthat::test_that("read.gt3x old format - timezone configuration (Europe/London) < timezone worn (Europe/Helsinki)", {
  gt3xdata_confidLon_wornHel <- read.gt3x(path, asDataFrame = TRUE, imputeZeroes = FALSE,
                                          desiredtz = tzHel, configtz = tzLon)
  testthat::expect_true(inherits(gt3xdata_confidLon_wornHel$time[1], "POSIXct"))
  testthat::expect_equal(gt3xdata_confidLon_wornHel$time[1], as.POSIXct("2012-06-27 12:54:00", tz = tzHel))
})
