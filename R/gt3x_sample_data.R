#' Path to read.gt3x package sample data
#'
#' @param index The index of a sample file to retrieve
#'
#' @examples
#' dir <- gt3x_datapath()
#'
#' @export
gt3x_datapath <- function(index = NULL) {
  datadir <- file.path(path.package("read.gt3x"), "extdata")
  if(!dir.exists(datadir)) stop("Data directory not found")
  # TODO: download data the first time this is called.
  if(!is.null(index)) {
    files <- list_gt3x(datadir)
    if(index > length(files)) stop("Index is larger than the number of sample files, which there are ", length(files))
    return(files[index])
  }
  datadir
}
