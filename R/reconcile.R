#' Merge and Reconcile Meter Data
#'
#' Merge multiple \code{meter_df} objects together so that the values can be
#' compared. This often requires aggregation or resampling of the original
#' values.
#' @name reconcile
#' @param ... \code{meter_df} objects, which will be merged and reconciled
#' @examples
#' library("units")
#'
#' x <- data.frame(time = seq.Date(as.Date("2013-1-1"), by = "+1 month", length.out = 12),
#'                 value = set_units(rnorm(12), kW))
#'
#' meter_df(x)
#' @export
reconcile <- function(...) {

  x <- list(...)
  is_meter_df <- vapply(x, function(x) inherits(x, "meter_df"), TRUE)
  stopifnot(all(is_meter_df))

  mdf_elec <- lapply(x, get_electricity)
  mdf_elec <- mdf_elec[!vapply(mdf_elec, is.null, TRUE)]

  mdf_water <- lapply(x, get_water)
  mdf_water <- mdf_water[!vapply(mdf_water, is.null, TRUE)]

  # combine and reconcile data with common units
  # TODO: check units\
  # TODO: handle offset point timestamps
  # TODO: handle mixed point timestamps and intervals

  mdf_elec_rec <- if (all(sapply(mdf_elec, time_ind_type) == "timestamp")) {
    Reduce(merge_all, mdf_elec)
  } else if (all(sapply(mdf_elec, time_ind_type) == "interval")) {
    reconcile_intervals(mdf_elec)
  } else {
    stop("combination of point timestamp and time interval is not currently supported", call. = FALSE)
  }

  mdf_water_rec <- if (all(sapply(mdf_water, time_ind_type) == "timestamp")) {
    Reduce(merge_all, mdf_water)
  } else if (all(sapply(mdf_water, time_ind_type) == "interval")) {
    reconcile_intervals(mdf_water)
  } else {
    stop("combination of point timestamp and time interval is not currently supported", call. = FALSE)
  }

  # combine and reconcile data with different units
  # TODO
  mdf_all <- list(mdf_elec_rec, mdf_water_rec)
  mdf_all_rec <- if (all(sapply(mdf_all, time_ind_type) == "timestamp")) {
    Reduce(merge_all, mdf_all)
  } else if (all(sapply(mdf_all, time_ind_type) == "interval")) {
    reconcile_intervals(mdf_all)
  } else {
    stop("combination of point timestamp and time interval is not currently supported", call. = FALSE)
  }

  mdf_all_rec
}

get_electricity <- function(mdf) {

  is_elec <- vapply(mdf[attr(mdf, 'meas_fields')],
                    function(x) units::as_cf(x) %in% getOption("ei.energy_units"),
                    TRUE)
  is_elec <- is_elec[is_elec]

  mdf_elec <- if (any(is_elec)) {
    mdf[, c(attr(mdf, 'time_field'), names(is_elec)), drop = FALSE]
  } else {
    NULL
  }

  mdf_elec
}

get_water <- function(mdf) {

  is_water <- vapply(mdf[attr(mdf, 'meas_fields')],
                    function(x) units::as_cf(x) %in% getOption("ei.water_units"),
                    TRUE)
  is_water <- is_water[is_water]

  mdf_water <- if (any(is_water)) {
    mdf[, c(attr(mdf, 'time_field'), names(is_water)), drop = FALSE]
  } else {
    NULL
  }

  mdf_water
}

reconcile_intervals <- function(x) {
  time_indices <- lapply(x, get_time_ind)
  stop("reconciling time intervals not implemented yet", call. = FALSE)
  NULL
}

