
.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("memoise", quietly = TRUE))
    packageStartupMessage("install package 'memoise' for faster network accumulation calculations")

  invisible()
}

# define units
mg <- units::make_unit("mg")
af <- units::make_unit("af")
units::install_conversion_constant(from = "mg", to = "af", const = 3.068883245971575)

mgd <- units::make_unit("mgd")

# default units
options(ei.water_units = c("mg", "af", "mgd"),
        ei.energy_units = c("kwh"))
