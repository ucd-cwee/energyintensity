#' Create \code{meter_df} object
#'
#' Create \code{meter_df}, which extends data.frame-like objects with standardized meter data series
#' @name meter_df
#' @param ... column elements to be binded into an \code{meter_df} object or a single \code{list} or \code{data.frame} with required columns.
#' @param stringsAsFactors logical; logical: should character vectors be converted to factors?  The `factory-fresh' default is \code{TRUE}, but this can be changed by setting \code{options(stringsAsFactors = FALSE)}.
#' @examples
#' # meter_df()
#' @export

meter_df <- function(..., stringsAsFactors = default.stringsAsFactors()) {

  x <- list(...)

  # TODO:
  #   validate

  df <- data.frame(x, stringsAsFactors = stringsAsFactors)

  structure(df,
            class = c("meter_df", class(df)))
}
