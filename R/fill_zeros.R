#' Fill Zeroes to Last Observation Carried Forward
#'
#' @param x Object of class `activity`/`activity_df` or a `data.frame`,
#' with columns `X`/`Y`/`Z`
#' @returns A similar object passed in with XYZ filled in with last observation
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

  x
}

#' @export
#' @rdname fill_zeros
fill_zeroes = fill_zeros
