#' Get return levels
#' @importFrom evd qgev
#' @export
GetReturnLevels <- function(period, loc, scale, shape) {
  tmp <- qgev(1 - 1/period, loc, scale, shape)
  names(tmp) <- paste0("y", period)
  tmp
}


#' Climatology of Return Levels
#' @export
ReturnLevelClimatology <- function(data, type,
                                   windowSize = 15L,
                                   returnPeriod = c(2, 5, 10, 25, 50, 100),
                                   kLoc = 10, kScale = 10, kShape = 0L) {
  tmp <- as.data.table(data)
  tmp[, displayDate := as.Date(paste(2016, substr(date, 6, 10), sep="-"))]

  tmp[, ext := GetWindowExtremes(tx, windowSize, type)]

  tmp <- DetermineShape(tmp, kShape)

  estimates <- DetermineLocationAndScale(tmp, kLoc, kScale)

  returnLevels <- estimates[,  as.list(GetReturnLevels(returnPeriod, loc, scale, shape)), by = displayDate]
  returnLevels <- melt(returnLevels, id.vars = "displayDate", variable.name = "returnPeriod")

  list(estimates = estimates, returnLevels = returnLevels)
}
