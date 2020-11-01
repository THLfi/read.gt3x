test_that("Index out of bounds no longer an issue", {
  # Fixes https://github.com/THLfi/read.gt3x/issues/19
  # check
  testthat::skip_on_cran()
  destfile = tempfile(fileext = ".gt3x.gz")
  download.file("https://ndownloader.figshare.com/files/24349628",
                destfile = destfile)
  testthat::expect_silent({
    res = read.gt3x(destfile, imputeZeroes = TRUE, verbose = FALSE)
  })
  hdr = attributes(res)$header
  res = read.gt3x(destfile,
                  imputeZeroes = TRUE,
                  verbose = TRUE,
                  asDataFrame = TRUE )
  testthat::expect_true(
    abs(hdr$`Last Sample Time` -  res$time[nrow(res)]) < 1/hdr$`Sample Rate`
  )
})
