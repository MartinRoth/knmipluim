#' Calculate GEV parameters
#'
#' @importFrom extRemes fevd
#' @export
GetGevParams <- function(x) {
  tmp <- fevd(x, type = "GEV", method = "Lmoments")
  tmp$results
}
