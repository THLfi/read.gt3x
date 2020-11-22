path <- system.file(
  "extdata", "TAS1H30182785_2019-09-17.gt3x",
  package = "read.gt3x")

destroy_field = function(gt3x, field = "Acceleration Scale",
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
testthat::context("Need to get rid of certain fields for coverage")

testthat::test_that("Trying without Acceleration Scale", {
  testthat::skip_if_not_installed("zip")
  run_file = destroy_field(path)
  result = parse_gt3x_info(run_file)
  testthat::expect_equal(result$`Acceleration Scale`, 256L)
  testthat::expect_true(get_n_samples(result) == 240500L)
  run_file = destroy_field(path, field = c("Last Sample Time", "Download Date", "Sample Rate"))
  result = parse_gt3x_info(run_file)
  testthat::expect_warning({
    out = get_n_samples(result)
  })
  testthat::expect_true(attr(out, "bad"))
  attr(out, "bad") = NULL
  testthat::expect_equal(out, 8.64e+08)


  run_file = destroy_field(path,
                           field = c("Last Sample Time"),
                           replace_field = list("Stop Date" = "0",
                                                "Sample Rate" = "Hey"))
  result = parse_gt3x_info(run_file)
  testthat::expect_warning({
    out = get_n_samples(result)
  })

  run_file = destroy_field(path, field = "all")

  testthat::expect_error({
  result = unzip.gt3x(run_file, verbose = TRUE)
  })

})
