
testthat::context("Reading New data")
csvfile <- "../TAS1H30182785_2019-09-17.csv"
gt3xfile <- "../TAS1H30182785_2019-09-17.gt3x"


# Read ActiLife raw data output CSV
csvdata <- as.data.frame(data.table::fread(csvfile))
gt3xdataZeroes <- read.gt3x(gt3xfile, imputeZeroes = TRUE)
gt3xdata <- read.gt3x(gt3xfile)
tfile = tempfile(fileext = ".gt3x")
file.copy(gt3xfile, tfile)

testthat::test_that("read.gt3x reads the first second of data correctly", {
  testthat::expect_true(all(unlist(head(csvdata, 100)) == unlist(head(gt3xdata, 100))))
})

testthat::test_that("No lags in gt3x data.frame timestamps after imputation", {
  gt3xdf <- as.data.frame(gt3xdataZeroes)
  diffs <- diff(gt3xdf$time)
  testthat::expect_true(!any(diffs > 1))
})


testthat::test_that("Number of missing values correctly attributed", {
  nmis <-   sum(attr(gt3xdata, "missingness")$n_missing)
  testthat::expect_true(nrow(gt3xdata) + nmis == nrow(csvdata))
})

testthat::test_that("Removing the gt3x works fine", {
  testthat::expect_true(file.exists(tfile))
  res = unzip_single_gt3x(tfile, remove_original = TRUE, verbose = TRUE)
  testthat::expect_is(res, "character")
  testthat::expect_false(file.exists(tfile))
  tfile2 = tempfile()
  res = unzip_single_gt3x(tfile2)
  testthat::expect_null(res)

  testthat::expect_message({
    res = unzip_single_gt3x(NULL)
  }, "Unzipping faile")
  testthat::expect_null(res)
})


