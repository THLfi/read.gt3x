testthat::context("Reading New data")
csvfile <- "../TAS1H30182785_2019-09-17.csv"
gt3xfile <- "../TAS1H30182785_2019-09-17.gt3x"

# Read ActiLife raw data output CSV
csvdata <- as.data.frame(data.table::fread(csvfile))
gt3xdataZeroes <- read.gt3x(gt3xfile, imputeZeroes = T)
gt3xdata <- read.gt3x(gt3xfile)

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

