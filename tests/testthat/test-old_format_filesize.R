url <- paste0("https://ndownloader.figshare.com/files/21855801")
path <- tempfile(fileext = ".gt3x.gz")
# file is PU2_NEO1B41100255_2016-04-21.gt3x.gz
dl <- utils::download.file(url, destfile = path, mode = "wb")


testthat::test_that("Old format file_size determines nrow", {
  testthat::expect_warning({
    res <- read.gt3x::read.gt3x(path)
  }, regexp = "Estimated samples")
  testthat::expect_is(res, "activity")
  testthat::expect_is(res, "matrix")
  testthat::expect_equal(colnames(res), c("X", "Y", "Z"))
  testthat::expect_equal(nrow(res), 18144012L)
  rm(res)
})

