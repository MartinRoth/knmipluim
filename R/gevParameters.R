#' Calculate GEV Parameters
#'
#' @description Calculate GEV parameters using MLE
#' @param x numeric vector containing the data
#'
#' @importFrom evd fgev
#' @export
GetGevParams <- function(x) {
  tmp <- fgev(x, std.err=FALSE, warn.inf = FALSE)
  as.list(tmp$estimate)
}

#' Calculate GEV Parameters (Fixed Shape)
#'
#' @description Calculate GEV parameters using MLE
#' @param x numeric vector containing the data
#' @param shape numeric value
#'
#' @importFrom evd fgev
#' @export
GetGevParamsFixedShape <- function(x, shape) {
  tmp <- fgev(x, shape = shape, std.err = FALSE, warn.inf = FALSE)
  as.list(tmp$estimate)
}
