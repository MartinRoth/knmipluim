#' Plot estimates
#' @import ggplot2
#' @export
PlotEstimates <- function(x) {
  values <- x$estimates[, 1:4, with = FALSE]
  oldValues <- x$estimates[, c(1, 5 : 7), with = FALSE]
  setnames(oldValues, names(values))
  values <- melt(values, id.vars = "displayDate", variable.name = "parameter")
  oldValues <- melt(oldValues, id.vars = "displayDate", variable.name = "parameter")
  ggplot(oldValues, aes(x = displayDate, y = value)) + geom_point() +
    geom_line(col=2, data = values) +
    facet_wrap(~parameter, scales="free_y", nrow=3) +
    scale_x_date(date_labels = "%b %d")
}

#' Plot return levels
#' @import ggplot2
#' @export
PlotReturnLevels <- function(x) {
  ggplot(x$returnLevels, aes(x = displayDate, y = value, col = returnPeriod)) + geom_line() +
    scale_x_date(date_labels = "%b %d")
}
