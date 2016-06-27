#' Get return levels
#' @importFrom evd qgev
#' @export
GetReturnLevels <- function(period, loc, scale, shape, type) {
  tmp <- qgev(1 - 1/period, loc, scale, shape)
  if (type == "min") {
    tmp <- -tmp
  }
  names(tmp) <- paste0("y", period)
  tmp
}


#' Climatology of Return Levels
#' @export
ReturnLevelClimatology <- function(data, var, type,
                                   windowSize = 15L,
                                   returnPeriod = c(2, 5, 10, 25, 50, 100),
                                   kLoc = 10, kScale = 10, kShape = 0L) {
  tmp <- as.data.table(data)
  tmp[, displayDate := as.Date(paste(2016, substr(date, 6, 10), sep="-"))]

  tmp[, ext := GetWindowExtremes(get(var), windowSize, type)]

  tmp <- DetermineShape(tmp, kShape)

  estimates <- DetermineLocationAndScale(tmp, kLoc, kScale)

  returnLevels <- estimates[,  as.list(GetReturnLevels(returnPeriod, loc, scale, shape, type)), by = displayDate]
  returnLevels <- melt(returnLevels, id.vars = "displayDate", variable.name = "returnPeriod")

  list(type = type, data = data, windowSize = windowSize, estimates = estimates, returnLevels = returnLevels)
}
