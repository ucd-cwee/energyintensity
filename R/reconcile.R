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
  # TODO

  # combine and reconcile data with different units
  # TODO

  # FIXME: simplest case is to just merge on common timestamps
  merge(mdf_elec[[1]], mdf_water[[1]], by = "time")
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

# reconcile_same


# reconcile_different
