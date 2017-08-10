#' Create \code{meter_df} object
#'
#' Create \code{meter_df}, which is essentially a data.frame with standardized meter data series
#' @name meter_df
#' @param ... column elements to be binded into an \code{meter_df} object or a \code{data.frame} with required columns. One element must be a time index and at least one should have measured values with units set, see \code{set_units}
#' @param stringsAsFactors logical; logical: should character vectors be converted to factors?  The `factory-fresh' default is \code{TRUE}, but this can be changed by setting \code{options(stringsAsFactors = FALSE)}.
#' @examples
#' library("units")
#'
#' x <- data.frame(time = seq.Date(as.Date("2013-1-1"), by = "+1 month", length.out = 12),
#'                 value = set_units(rnorm(12), kW))
#'
#' meter_df(x)
#' @export
meter_df <- function(..., stringsAsFactors = default.stringsAsFactors()) {

  x <- list(...)
  if (length(x) == 1L && inherits(x[[1L]], "data.frame"))
    x = x[[1L]]

  # validate date/time
  has_time <- sapply(x, function(y) inherits(y, c("Date", "POSIXt", "Interval")))
  if (sum(has_time) != 1) stop("exactly one field should a have time/date stamp, or time/date-inteval values", call. = FALSE)

  # validate units
  has_units <- sapply(x, function(y) inherits(y, "units"))
  if (!any(has_units)) stop("at least one field should have measured values with units set", call. = FALSE)

  # for now, constrain to known subset of units
  # TODO: review if this restriction can be safely relaxed
  all_units <- sapply(x[has_units], units::as_cf)
  known_units <- c(getOption("ei.energy_units"), getOption("ei.water_units"))
  unknown_units <- setdiff(all_units, known_units)
  if (length(unknown_units) > 0) stop("invalid units: ", unknown_units, call. = FALSE)

  # time and measurement units should be consistent
  # TODO: convert with warning
  is_rate <- sapply(x[has_units], function(y) {
    denom <- units(y)$denominator
    denom_is_dur <- denom %in% c("s", "second",
                                 "m", "minute",
                                 "h", "hour",
                                 "d", "day",
                                 "m", "month",
                                 "y", "year")
    denom_is_dur <- ifelse(length(denom_is_dur) == 0, FALSE, denom_is_dur)
    watts <- try(units::set_units(y, "W"), silent = TRUE)
    is_watts <- !inherits(watts, "try-error")
    denom_is_dur | is_watts
  })

  is_interval <- inherits(x[[which(has_time)]], "Interval")
  if (is_interval & any(is_rate)) stop("time is interval, but measurement is rate", call. = FALSE)
  if (!is_interval & !any(is_rate)) stop("time is point, but measurement is 'volume'", call. = FALSE)

  df <- data.frame(..., stringsAsFactors = stringsAsFactors)
  if (inherits(x, 'tbl_df')) df <- tibble::as_tibble(df)

  # meter_df
  structure(df,
            class = c("meter_df", class(df)),
            time_field = names(x)[has_time],
            meas_fields = names(x)[has_units])
}

#' @export
print.meter_df <- function(x, ...) {
  NextMethod()
}

#' @importFrom graphics lines plot
#' @export
plot.meter_df <- function(x, y, ..., type = 'l') {

  stopifnot(missing(y))

  t_col <- which(sapply(x, function(y) inherits(y, c("Date", "POSIXt", "Interval"))))
  t_vals <- x[[t_col]]

  meas_cols <- which(sapply(x, function(y) inherits(y, "units")))
  meas_vals <- x[meas_cols]

  if (length(meas_vals) == 0) {
    plot(t_vals, type = type, ...)
  } else {

    # first measure field
    if (inherits(t_vals, "Interval")) {
      plot(lubridate::int_start(t_vals), meas_vals[[1]], type = 'n', ...)
      for (i in seq_along(t_vals)) {
        lines(rbind.data.frame(
          c(lubridate::int_start(t_vals[i]), meas_vals[[1]][i]),
          c(lubridate::int_end(t_vals[i]), meas_vals[[1]][i])
        ))
      }
    } else {
      plot(t_vals, meas_vals[[1]], type = type, ...)
    }

    # additional measure fields
    for (i in seq_along(meas_vals)[-1]) {
      if (inherits(t_vals, "Interval")) {
        for (j in seq_along(t_vals)) {
          lines(data.frame(x = c(lubridate::int_start(t_vals[j]),
                                 lubridate::int_end(t_vals[j])),
                           y = rep(meas_vals[[i]][j], 2)))
        }
      } else {
        lines(t_vals, meas_vals[[i]], type = type)
      }
    }
  }

  invisible(NULL)
}

#' @export
"[.meter_df" = function(x, i, j, ..., drop = FALSE) {
	NextMethod()
}

#' @export
"[[.meter_df" = function(x, i, j, ...) {
	NextMethod()
}

#' @export
"$.meter_df" = function(x, name) {
	NextMethod()
}

#' @export
"$<-.meter_df" = function(x, name, value) {
  NextMethod()
}

#' @export
"[[<-.meter_df" = function(x, i, j, ..., value) {
	NextMethod()
}
