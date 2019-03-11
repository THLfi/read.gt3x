

#' Parse GT3X info.txt file
#'
#' @param path Path to a .gt3x file or an unzipped gt3x directory
#'
#' @family gt3x-parsers
#'
#' @examples
#' gt3xfile <- gt3x_datapath(1)
#' parse_gt3x_info(gt3xfile)
#'
#' @export
parse_gt3x_info <- function(path, tz = "GMT") {
  if(is_gt3x(path))
    path <- unzip.gt3x(path)
  infotxt <- readLines(file.path(path, "info.txt"))
  infotxt <- strsplit(infotxt, split = ": ")
  infomatrix <- do.call("rbind", infotxt)
  values <- infomatrix[, 2]
  names(values) <- infomatrix[, 1]
  info <- as.list(values)
  info$`Sample Rate` <- as.numeric(info$`Sample Rate`)
  info$`Start Date` <- ticks2datetime(info$`Start Date`, tz = tz)
  info$`Stop Date` <- ticks2datetime(info$`Stop Date`, tz = tz)
  info$`Last Sample Time` <- ticks2datetime(info$`Last Sample Time`, tz = tz)
  info$`Download Date` <- ticks2datetime(info$`Download Date`, tz = tz)
  info$`Acceleration Scale` <- as.numeric(info$`Acceleration Scale`)
  structure(info, class = c("gt3x_info", class(info)))
}

#' Print the contents of the info.txt file in a gt3x folder
#'
#' @param x gt3x_info object returned by parse_gt3x_info()
#'
#' @family gt3x-parsers
#'
#' @export
print.gt3x_info <- function(x) {
  cat("GT3X information\n")
  str(x, give.head = FALSE, no.list=TRUE)
}

