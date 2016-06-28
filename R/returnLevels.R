# Get return levels
#' @importFrom evd qgev
GetReturnLevels <- function(period, loc, scale, shape, type) {
  tmp <- qgev(1 - 1/period, loc, scale, shape)
  if (type == "min") {
    tmp <- -tmp
  }
  names(tmp) <- paste0("y", period)
  tmp
}


#' Climatology of Return Levels
#'
#' @param data data (needs date and variable field)
#' @param var  string variable e.g. tx
#' @param type string c('max', 'min')
#' @param windowSize integer
#' @param returnPeriod vector return period
#' @param kLoc knots for location (0 is fixed)
#' @param kScale knots for scale (0 is fixed)
#' @param kShape knots for shape (0 is fixed)
#'
#' @export
ReturnLevelClimatology <- function(data, var, type,
                                   windowSize = 15L,
                                   returnPeriod = c(2, 5, 10, 25, 50, 100),
                                   kLoc = 10, kScale = 10, kShape = 0L) {
  ext <- loc <- shape <- displayDate <- NULL
  tmp <- copy(data)
  tmp[, displayDate := as.Date(paste(2016, substr(date, 6, 10), sep="-"))]


  tmp[, ext := GetWindowExtremes(get(var), windowSize, type)]

  if (type == "max") {
    extremeObservations <- tmp[, max(get(var)), by = displayDate]
  } else {
    extremeObservations <- tmp[, min(get(var)), by = displayDate]
  }
  setnames(extremeObservations, 2, "extObs")

  tmp <- DetermineShape(tmp, kShape)

  estimates <- DetermineLocationAndScale(tmp, kLoc, kScale)

  returnLevels <- estimates[,  as.list(GetReturnLevels(returnPeriod, loc, scale, shape, type)), by = displayDate]
  returnLevels <- melt(returnLevels, id.vars = "displayDate", variable.name = "returnPeriod")

  list(type = type, data = data, windowSize = windowSize,
       estimates = estimates, returnLevels = returnLevels,
       extremes = extremeObservations)
}
