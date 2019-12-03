
csvfile <- "tests/TAS1H30182785 (2019-09-17).csv"
gt3xfile <- "tests/TAS1H30182785 (2019-09-17).gt3x"

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
