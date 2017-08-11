
col_has_units <- function(x) {
  vapply(x, function(y) inherits(y, "units"), TRUE)
}

get_time_ind <- function(x) {
  stopifnot(inherits(x, "meter_df"))

  x[[attr(x, "time_field")]]
}

time_ind_type <- function(x) {
  stopifnot(inherits(x, "meter_df"))

  td <- x[[attr(x, "time_field")]][1]

  if (inherits(td, "POSIXt") | inherits(td, "Date"))
    "timestamp"
  else if (inherits(td, "Interval"))
    "interval"
  else
    stop("invalid time index type", call. = FALSE)
}

merge_all <- function(x, y) {
  merge(x, y,
        by.x = attr(x, "time_field"),
        by.y = attr(y, "time_field"),
        all = TRUE)
}
