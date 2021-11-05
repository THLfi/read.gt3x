
url <- paste0("https://ndownloader.figshare.com/files/25557302")
path <- tempfile(fileext = ".gt3x.gz")
dl <- utils::download.file(url, destfile = path, mode = "wb")

destroy_field = function(path, field = "Acceleration Scale",
                         replace_field = list()) {
  path <- read.gt3x:::unzip_zipped_gt3x(path, cleanup = TRUE)

  tdir = tempfile()
  out = unzip(path, exdir = tdir, overwrite = TRUE)
  info_file = out[grepl("info.txt", out)]

  if (all(field %in% "all")) {
    out = out[!grepl("info.txt", out)]
  } else {
    info = readLines(info_file)
    info
    keys = sapply(strsplit(info, split = ":"), `[`, 1)
    info = info[!keys %in% field]
    if (length(replace_field) >0) {
      for (ifield in seq_along(replace_field)) {
        n =  names(replace_field)[ifield]
        ind = which(keys == n)
        info[ind] = paste0(n, ": ", replace_field[[ifield]])
      }
    }
    writeLines(info, info_file)
  }
  new_gt3x_file = tempfile(fileext = ".gt3x")
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(files = basename(out),
             zipfile = new_gt3x_file,
             root = tdir,
             include_directories = FALSE)
  }
  return(new_gt3x_file)
}

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


