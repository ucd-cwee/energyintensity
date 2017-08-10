
col_has_units <- function(x) {
  vapply(x, function(y) inherits(y, "units"), TRUE)
}
