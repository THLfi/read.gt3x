
csvfile <- "tests/TAS1H30182785 (2019-09-17).csv"
gt3xfile <- "tests/TAS1H30182785 (2019-09-17).gt3x"

# Read ActiLife raw data output CSV
csvdata <- as.data.frame(fread(csvfile))

testthat::test_that("read.gt3x reads the first second of data correctly", {
  gt3xdata <- read.gt3x(gt3xfile)
  testthat::expect_true(all(unlist(head(csvdata, 100)) == unlist(head(gt3xdata, 100))))
})
