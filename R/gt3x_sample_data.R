#' Path to read.gt3x package sample data
#'
#' @param index Integer. The index of a sample file to retrieve. If NULL (default) the path to the directory
#' including the sample files will be returned.
#'
#' @examples
#' dir <- gt3x_datapath()
#' gt3x_filename <- gt3x_datapath(1)
#'
#' @export
gt3x_datapath <- function(index = NULL) {
  homedir <-  path.expand('~')
  datadir <- file.path(homedir, ".read.gt3x-data")
  if(!dir.exists(datadir))
    dir.create(datadir)
  filenames <- gt3x_filename(index)
  for(filename in filenames) {
    if(!file.exists(file.path(datadir, filename)))
      gt3x_download(url = gt3x_url(filename=filename), destfile = file.path(datadir, filename))
  }
  if(!is.null(index)) {
    files <- list_gt3x(datadir)
    return(files[index])
  }
  datadir
}

#' Download a gt3xfile
gt3x_download <- function(url, destfile) {
  download.file(url = url, destfile = destfile, method = "auto", mode = "wb")
}

#' Get url of gt3x sample files
gt3x_url <- function(index = NULL, filename = NULL, version = "v0.1-alpha", baseurl = "https://github.com/THLfi/read.gt3x/releases/download") {
  dataurl <- gt3x_dataurl(version, baseurl)
  if(is.null(filename))
    filename <- gt3x_filename(index)
  file.path(dataurl, filename)
}

#' Get url of github release
gt3x_dataurl <- function(version = "v0.1-alpha", baseurl = "https://github.com/THLfi/read.gt3x/releases/download") {
  url <- file.path(baseurl, version)
}

#' Get sample gt3x filenames
gt3x_filename <- function(index = NULL) {
  files <- c("EE_left_29.5.2017-05-30.gt3x", "SS_left_19.5.2017-05-22.gt3x")
  if(is.null(index)) return(files)
  if(index > length(files)) stop("Index is larger than the number of available files")
  if(index < 1) stop("Index can't be less than 1")
  files[index]
}
