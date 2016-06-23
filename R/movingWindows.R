#' Moving window maxima
#' @param x numeric data vector
#' @param k integer window size (use e.g. 2L)
#' @param type string type of extreme
#' @description returns \code{max(x[(i - k):(i + k)])} for \code{i = 1,...,n} or (\code{min(...))}
#' @importFrom caTools runmax runmin
#' @importFrom assertthat assert_that
#' @importFrom assertive is_integer
#' @export
#' @examples
#' x <- c(1, 2, 3, 2, 5, 0, 2, 3, 8)
#' GetWindowExtremes(x, 2L, "max")
#' GetWindowExtremes(x, 2L, "min")
GetWindowExtremes <- function(x, k, type = c('max', 'min')) {
  #if(!is_integer(k)) stop("Error : is_integer(x = k) is not TRUE")
  assert_that(is_integer(k), length(k)==1)
  windowSize <- k + 1 + k
  switch (type,
    max = return(runmax(x, windowSize)),
    min = return(runmin(x, windowSize))
  )
  stop(paste("Method", type, "not defined."))
}

