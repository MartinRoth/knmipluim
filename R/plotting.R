#' Plot estimates
#' @param x knmipluim object
#' @import ggplot2
#' @export
PlotEstimates <- function(x) {
  displayDate <- value <- NULL
  values <- x$estimates[, 1:4, with = FALSE]
  oldValues <- x$estimates[, c(1, 5 : 7), with = FALSE]
  setnames(oldValues, names(values))
  values <- melt(values, id.vars = "displayDate", variable.name = "parameter")
  oldValues <- melt(oldValues, id.vars = "displayDate", variable.name = "parameter")
  ggplot(oldValues[displayDate != "2016-02-29"], aes(x = displayDate, y = value)) +
    geom_point() +
    geom_line(col=2, data = values[displayDate != "2016-02-29"]) +
    facet_wrap(~parameter, scales="free_y", nrow=3) +
    scale_x_date(date_labels = "%b %d") +
    xlab("Day of the year")
}

#' Plot return levels
#' @inheritParams PlotEstimates
#' @import ggplot2
#' @export
PlotReturnLevels <- function(x) {
  displayDate <- value <- returnPeriod <- extObs <- NULL
  ggplot(x$returnLevels, aes(x = displayDate, y = value, col = returnPeriod)) +
    geom_line() +
    geom_point(aes(y = extObs, col = NULL), data = x$extremes) +
    scale_x_date(date_labels = "%b %d") +
    xlab("Day of the year") + ylab(x$var) +
    guides(col=guide_legend(title="return period"))
}
