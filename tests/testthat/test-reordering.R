
idf = list(
  name_gt3x = "PU3_CLE2B21130054_2017-03-16.gt3x.gz",
  download_url_gt3x = "https://ndownloader.figshare.com/files/21855807",
  id = "PU3", serial = "CLE2B21130054",
  name_csv = "PU3_CLE2B21130054_2017-03-16.csv.gz",
  download_url_csv = "https://ndownloader.figshare.com/files/24488492")
print(idf)
download = function(url, name) {
  destfile = file.path(tempdir(), name)
  if (!file.exists(destfile)) {
    old <- options()         # code line i
    on.exit(options(old))    # code line i+1
    options(timeout = 120)
    download.file(url, destfile, mode = "wb")
  }
  destfile
}

sub_thing = function(hdr, string) {
  x = hdr[grepl(string, hdr)]
  x = gsub(string, "", x)
  x = trimws(x)
}


testthat::test_that("Reordering columns is right", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("lubridate")


  gt3x_file = download(idf$download_url_gt3x, idf$name_gt3x)

  read_acc_csv = function(file, ...) {
    hdr = readLines(file, n = 10)
    st = sub_thing(hdr, "Start Time")
    sd = sub_thing(hdr, "Start Date")
    format = sub(".*date format (.*) at.*", "\\1", hdr[1])
    if (format == "") {
      warning("No format for date in the header, using mdy")
      format = "mdy"
    } else {
      format = tolower(format)
      format = c(sapply(strsplit(format, "/"), substr, 1,1))
      format = paste(format, collapse = "")
    }
    all_formats = c("ydm", "dym", "ymd", "myd", "dmy", "mdy")
    stopifnot(format %in% all_formats)
    if (!requireNamespace("lubridate", quietly = TRUE)) {
      stop("readr must be installed for this function")
    }
    lubridate_func = paste0(format, "_hms")
    lubridate_func = getFromNamespace(lubridate_func, "lubridate")
    start_date = do.call(lubridate_func, args = list(paste0(sd, " ", st)))
    srate = as.numeric(sub(".*at (\\d*) Hz.*", "\\1", hdr[1]))

    if (!requireNamespace("readr", quietly = TRUE)) {
      stop("readr must be installed for this function")
    }
    suppressWarnings({
      df = readr::read_csv(
        file, skip = 10,
        col_types = readr::cols(
          .default = readr::col_double(),
          Date = readr::col_character(),
          Time = readr::col_time(format = "")
        ), ...)
    })
    readr::stop_for_problems(df)



    df$time = seq(0, nrow(df) - 1)/srate
    df$time = start_date + df$time
    class(df) = "data.frame"
    colnames(df) = trimws(sub("Accelerometer", "", colnames(df)))

    stopifnot(!anyNA(df$time))
    list(
      header = hdr,
      data = df
    )
  }
  csv_file = download(idf$download_url_csv, idf$name_csv)

  df = read_acc_csv(csv_file)
  # hdr = df$header
  df = df$data
  colnames(df) = sub("Accelerometer ", "", colnames(df))

  df = df[, "X", drop = FALSE]

  gc(); gc()
  act_df = read.gt3x(gt3x_file, verbose = TRUE,
                     asDataFrame = TRUE, imputeZeroes = TRUE)
  class(act_df) = "data.frame"
  act_df = act_df[, c("X", "Y", "Z")]

  good = df$X == act_df$X
  rm(df)
  testthat::expect_true(all(act_df[!good,c("X", "Y", "Z")] == 0))

})



testthat::test_that("Reordering columns is right", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("zip")

  gt3x_file = download(idf$download_url_gt3x, idf$name_gt3x)

  run_file = destroy_field(gt3x_file)
  testthat::expect_silent({
    result = parse_gt3x_info(run_file)
  })
  testthat::expect_equal(result$`Acceleration Scale`, 341L)
})

