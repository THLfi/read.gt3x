url = "https://github.com/THLfi/read.gt3x/files/3522749/GT3X%2B.01.day.gt3x.zip"
destfile = tempfile(fileext = ".zip")
dl = utils::download.file(url, destfile = destfile)
gt3x_file = utils::unzip(destfile, exdir = tempdir())
gt3x_file = gt3x_file[!grepl("__MACOSX", gt3x_file)]
path = gt3x_file

testthat::test_that("Reading in Old format works", {
  res = read.gt3x::read.gt3x(path)
  testthat::expect_is(res, "activity")
  testthat::expect_is(res, "matrix")
  testthat::expect_equal(colnames(res), c("X", "Y", "Z"))

  cm = unname(colMeans(res))
  testthat::expect_equal(cm, c(0.447592941851854,
                               -0.228402625555557, 0.11958707074074))
  testthat::expect_equal(unname(res[4823, "X"]), 0.528)

  all_attr = attributes(res)
  testthat::expect_true(all_attr$old_version)
  testthat::expect_equal(all_attr$sample_rate, 30)
  rm(res)

})


testthat::test_that("Converting Old to Data.frame", {
  res = read.gt3x::read.gt3x(path, asDataFrame = TRUE)
  testthat::expect_is(res, "activity_df")
  testthat::expect_is(res, "data.frame")
  testthat::expect_equal(colnames(res), c("X", "Y", "Z", "time"))

  cm = unname(colMeans(res[,1:3]))
  testthat::expect_equal(cm, c(0.447592941851854,
                               -0.228402625555557, 0.11958707074074))
  testthat::expect_equal(
    res$time[48], structure(1340794445.23333,
                            class = c("POSIXct", "POSIXt"), tzone = "GMT"))
  testthat::expect_equal(res$X[4823], 0.528)

  all_attr = attributes(res)
  testthat::expect_true(all_attr$old_version)
  testthat::expect_equal(all_attr$sample_rate, 30)

})
