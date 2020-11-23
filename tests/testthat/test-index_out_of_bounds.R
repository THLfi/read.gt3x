testthat::test_that("Index out of bounds no longer an issue", {
  # Fixes https://github.com/THLfi/read.gt3x/issues/19
  # check
  testthat::skip_on_cran()
  destfile = tempfile(fileext = ".gt3x.gz")
  res = download.file("https://ndownloader.figshare.com/files/24349628",
                      destfile = destfile, mode = "wb")
  testthat::expect_equal(res, 0L)
  testthat::expect_silent({
    res = read.gt3x(destfile, imputeZeroes = TRUE, verbose = FALSE)
  })
  hdr = attributes(res)$header
  rm(res)
  res = read.gt3x(destfile,
                  imputeZeroes = TRUE,
                  verbose = TRUE,
                  asDataFrame = TRUE )
  testthat::expect_true(
    abs(hdr$`Last Sample Time` -  res$time[nrow(res)]) < 1/hdr$`Sample Rate`
  )
  rm(res)
})


testthat::test_that("Negative Missing values fixed", {
  # Fixes https://github.com/THLfi/read.gt3x/issues/18
  # check
  testthat::skip_on_cran()

  destfile = tempfile(fileext = ".gt3x.gz")
  res = download.file("https://ndownloader.figshare.com/files/24319343",
                      destfile = destfile, quiet = FALSE, mode = "wb")
  testthat::expect_equal(res, 0L)

  testthat::expect_warning({
    x = read.gt3x(destfile)
  })
  at = attributes(x)
  rm(x)
  head(at$missingness)
  testthat::expect_false(any(at$missingness$n_missing < 0))
  testthat::expect_warning({
    # if asDataFrame = TRUE, memory issues on Appveyor
    x = read.gt3x(destfile, imputeZeroes = TRUE)
  })
  rm(x)
  testthat::expect_warning({
    x = read.gt3x(destfile, verbose = 3, debug = TRUE)
  })
  rm(x)
})

