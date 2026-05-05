#' Replace all-zero XYZ values with last non-zero observation
#'
#' @param x Object of class `activity`/`activity_df` or a `data.frame`,
#' with columns `X`/`Y`/`Z`
#' @returns A similar object passed in, where rows with `X == 0 & Y == 0 & Z == 0`
#' have their XYZ values replaced by the last non-zero observation carried forward
#'
#' @examples
#' df <- data.frame(
#'   X = c(0, 1, 0, 2),
#'   Y = c(0, 3, 0, 4),
#'   Z = c(0, 5, 0, 6)
#' )
#' 
#' # Fill runs of all-zero rows using the last non-zero observation
#' fill_zeros(df)
#'
#' @export
fill_zeros = function(x) {
  x$all_zero = x$X == 0 & x$Y == 0 & x$Z == 0
  x$X = ifelse(x$all_zero, NA_real_, x$X)
  x$Y = ifelse(x$all_zero, NA_real_, x$Y)
  x$Z = ifelse(x$all_zero, NA_real_, x$Z)
  x$all_zero = NULL

  x$X = vctrs::vec_fill_missing(x$X, direction = "down")
  x$Y = vctrs::vec_fill_missing(x$Y, direction = "down")
  x$Z = vctrs::vec_fill_missing(x$Z, direction = "down")


  x$X[is.na(x$X)] = 0
  x$Y[is.na(x$Y)] = 0
  x$Z[is.na(x$Z)] = 0

  x
}

#' @export
#' @rdname fill_zeros
fill_zeroes = fill_zeros
