
.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("memoise", quietly = TRUE))
    packageStartupMessage("install package 'memoise' for faster network accumulation calculations")

  invisible()
}

# default units
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.ei <- list(
    ei.water_units = c("US_liquid_gallon", "liquid_gallon", "gallon",
                       "MUS_liquid_gallon", "Mliquid_gallon", "Mgallon",
                       "acre_foot",
                       "Mgallon d-1", "Mgallon day-1"),
    ei.energy_units = c("kW", "kiloW", "kWatt", "kiloWatt",
                        "h kW", "h kiloW", "h kWatt", "h kiloWatt",
                        "hour kW", "hour kiloW", "hour kWatt", "hour kiloWatt")
  )
  toset <- !(names(op.ei) %in% names(op))
  if (any(toset)) options(op.ei[toset])

  invisible()
}
