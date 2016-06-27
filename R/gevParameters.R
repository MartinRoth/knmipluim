#' Calculate GEV Parameters
#'
#' @description Calculate GEV parameters using MLE
#' @param x numeric vector containing the data
#'
#' @importFrom evd fgev
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
GetGevParamsFixedShape <- function(x, shape) {
  shape <- unique(shape)
  stopifnot(length(shape) == 1)
  tmp <- fgev(x, shape = shape, std.err = FALSE, warn.inf = FALSE)
  c(as.list(tmp$estimate), shape = shape)
}


# Get GEV Shape Parameter
#' @importFrom evd fgev
GetGevShape <- function(x) {
  tmp <- fgev(x, std.err=FALSE, warn.inf = FALSE)
  as.list(tmp$estimate[3])
}

# Determine GEV Shape Parameter
#' @import data.table
#' @import mgcv
DetermineShape <- function(data, k = 0L) {
  ext <- shape <- displayDate <- oldShape <- NULL
  tmp <- data[, GetGevShape(ext), by = displayDate]
  tmp[, oldShape := shape]
  if (k == 0L) {
    tmp[, shape := mean(shape)]
  } else {
    tmp[, shape := gam(shape ~ s(as.numeric(displayDate), bs="cc", k = k))$fitted.values]
  }
  merge(data, tmp, by = "displayDate")
}

# Determine GEV Location and Scale
DetermineLocationAndScale <- function(data, kLoc = 4L, kScale = 4L) {
  ext <- shape <- displayDate <- oldLoc <- oldScale <- oldShape <- loc <- NULL
  tmp <- data[, GetGevParamsFixedShape(ext, shape), by = displayDate]
  tmp[, oldLoc := loc]
  tmp[, oldScale := scale]
  tmp[, oldShape := data[, unique(oldShape), by = displayDate]$V1]
  if (kLoc == 0L) {
    tmp[, loc := mean(loc)]
  } else {
    tmp[, loc := gam(loc ~ s(as.numeric(displayDate), bs="cc", k = kLoc))$fitted.values]
  }
  if (kScale == 0L) {
    tmp[, scale := mean(scale)]
  } else {
    tmp[, scale := gam(scale ~ s(as.numeric(displayDate), bs="cc", k = kScale))$fitted.values]
  }
  tmp
}
