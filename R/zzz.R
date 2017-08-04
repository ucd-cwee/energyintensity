
.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("memoise", quietly = TRUE))
    packageStartupMessage("install package 'memoise' for faster network accumulation calculations")

  invisible()
}

# define units
MG <- units::make_unit("MG")    # million gallons
AF <- units::make_unit("AF")    # acre-feet
units::install_conversion_constant(from = "MG", to = "AF", const = 3.068883245971575)

# default units
options(ei.water_units = c("MG", "AF", "MG d-1"),
        ei.energy_units = c("h kW"))
