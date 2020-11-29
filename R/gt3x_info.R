

#' Parse GT3X info.txt file
#'
#' @param path Path to a .gt3x file or an unzipped gt3x directory
#' @param tz timezone, passed to \code{\link{ticks2datetime}}
#' @family gt3x-parsers
#'
#' @examples
#' gt3xfile <-
#'   system.file(
#'     "extdata", "TAS1H30182785_2019-09-17.gt3x",
#'     package = "read.gt3x")
#' parse_gt3x_info(gt3xfile)
#'
#' \dontrun{
#' gt3xfile <- gt3x_datapath(1)
#' parse_gt3x_info(gt3xfile)
#' }
#'
#' @export
parse_gt3x_info <- function(path, tz = "GMT") {
  path <- unzip_zipped_gt3x(path, cleanup = TRUE)
  if (is_gt3x(path)) {
    path <- unzip.gt3x(
      path,
      check_structure = FALSE,
      location = tempdir(),
      verbose = FALSE)
    on.exit(unlink(path, recursive = TRUE))
  }
  infotxt <- readLines(file.path(path, "info.txt"))
  infotxt <- strsplit(infotxt, split = ": ")
  infomatrix <- do.call("rbind", infotxt)
  values <- infomatrix[, 2]
  names(values) <- infomatrix[, 1]
  info <- as.list(values)
  info$`Serial Prefix` <- substr(info$`Serial Number`, 1, 3)
  info$`Sample Rate` <- as.numeric(info$`Sample Rate`)
  info$`Start Date` <- ticks2datetime(info$`Start Date`, tz = tz)
  info$`Stop Date` <- ticks2datetime(info$`Stop Date`, tz = tz)
  info$`Last Sample Time` <- ticks2datetime(info$`Last Sample Time`, tz = tz)
  info$`Download Date` <- ticks2datetime(info$`Download Date`, tz = tz)
  info$`Acceleration Scale` <- as.numeric(info$`Acceleration Scale`)

  if (old_version(info)) {
    if (length(info$`Acceleration Scale`) == 0) {
      info$`Acceleration Scale` <- 341L
    }
    if (length(info$`Acceleration Min`) == 0) {
      info$`Acceleration Min` <- "-6.0"
    }
    if (length(info$`Acceleration Max`) == 0) {
      info$`Acceleration Max` <- "6.0"
    }
  }
  pref = info$`Serial Prefix`
  prefs = c("NEO", "CLE", "MRA", "MOS", "TAS")
  # Trying to fix https://github.com/THLfi/read.gt3x/issues/22
  if (length(info$`Acceleration Scale`) == 0) {
    if (is.null(pref) || length(pref) == 0 || !pref %in% prefs) {
      warning("Acceleration Scale unknown from prefix, using 341")
      pref = NA
      scale = 341L
    }
    if (pref %in% c("NEO", "CLE", "MRA")) {
      scale = 341L
    }
    if (pref %in% c("MOS", "TAS")) {
      scale = 256L
    }
    info$`Acceleration Scale` <- scale
  }

  if (length(info$`Acceleration Max`) == 0 ||
      length(info$`Acceleration Min`) == 0
      ) {
    if (is.null(pref) || length(pref) == 0 || !pref %in% prefs) {
      pref = NA
      scale = NULL
    }
    if (pref %in% c("NEO", "CLE", "MRA")) {
      scale = 6
    }
    if (pref %in% c("MOS", "TAS")) {
      scale = 8
    }
    info$`Acceleration Min` <- as.character(-1*scale)
    info$`Acceleration Max` <- as.character(scale)
  }

  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  {
    abs(x - round(x)) < tol
  }

  if (is.wholenumber(info$`Acceleration Scale`)) {
    info$`Acceleration Scale` <- as.integer(info$`Acceleration Scale`)
  }

  structure(info, class = c("gt3x_info", class(info)))
}

old_version <- function(info) {
  firmware_version <- info$Firmware
  firmware_version <- package_version(firmware_version)
  hdr <- info$`Serial Prefix`
  ret <- hdr %in% c("MRA", "NEO") &
    firmware_version <= package_version("2.5.0")
  return(ret)
}

#' Print the contents of the info.txt file in a gt3x folder
#'
#' @param x gt3x_info object returned by parse_gt3x_info()
#' @param ... not used
#'
#' @family gt3x-parsers
#'
#' @export
print.gt3x_info <- function(x, ...) {
  cat("GT3X information\n")
  str(x, give.head = FALSE, no.list = TRUE)
}
