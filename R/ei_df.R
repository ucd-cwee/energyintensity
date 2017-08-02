#' Convert foreign object to an \code{ei_df} object
#' @param x object to be converted into an object class \code{ei_df}
#' @export
as.ei_df = function(x, ...) UseMethod("as.ei_df")

#' @name as.ei_df
#'
#' @param interval names or numbers of the \code{interval} column
#' @param energy names or numbers of the numeric columns holding energy data
#' @param water names or numbers of the numeric columns holding water data
#' @param ei name or number of the numeric columns holding ei data
#' @param ... passed on to \link{ei_df}, might included \code{method}
#'
#' @examples
#' library(units)
#' x <- data.frame(water = set_units(1:10, mg),
#'                 energy = set_units(rnorm(10), kWh))
#' as.ei_df(x)
#' @export
as.ei_df.data.frame <- function(x, ..., intervals, energy, water, ei) {

  # try to guess the required columns
  if (missing(intervals)) {
    intervals <- vapply(x, function(x) inherits(x, "interval"), TRUE)
    if (!any(intervals)) stop("no interval column found")
    if (sum(intervals) > 1) {
      warning("multiple interval columns found, using first...")
      intervals <- which(intervals)[1]
    }
  }
  int_data <- x[[intervals]]

  if (missing(energy)) {
    energy <- vapply(x, function(x) units(x) %in% getOption("ei.energy_units"), TRUE)
    if (!any(energy)) stop("no energy data found")
    if (sum(energy) > 1) {
      warning("multiple energy columns found, using first...")
      energy <- which(energy)[1]
    }
  }
  energy_data <- x[[energy]]

  if (missing(water)) {
    water <- vapply(x, function(x) units(x) %in% getOption("ei.water_units"), TRUE)
    if (!any(water)) stop("no water data found")
    if (sum(water) > 1) {
      warning("multiple water columns found, using first...")
      water <- which(water)[1]
    }
  }
  water_data <- x[[water]]

  # call constructor
  ei_df(intervals = int_data,
        energy = energy_data,
        water = water_data,
        ei = ei_df,
        ...)
}

#' @name as.ei_df
#' @export
as.ei_df.ei_df <- function(x, ...) x

#' Create \code{ei_df} object
#'
#' Create \code{ei_df}, which extends data.frame-like objects with standardized energy intensity data series
#' @name ei_df
#' @param ... column elements to be binded into an \code{ei_df} object or a single \code{list} or \code{data.frame} with required columns.
#' @param stringsAsFactors logical; logical: should character vectors be converted to factors?  The `factory-fresh' default is \code{TRUE}, but this can be changed by setting \code{options(stringsAsFactors = FALSE)}.
#' @examples
#' # ei_df()
#' @export
ei_df <- function(..., stringsAsFactors = default.stringsAsFactors()) {

  x <- list(...)

  structure(x,
            class = c("ei_data", class(x)))
}
