# sample meter data from a pump station

library(energyintensity)

set.seed(5)

# date intervals
st <- ymd("2015-01-01")
int <- interval(seq(st, st + months(11), by = "+1 month"),
                seq(st + months(1), st + months(12), by = "+1 month"))

# energy series
pst1_energy <- meter_df(interval = int,
                        energy = set_units(rnorm(12), kW*h))

devtools::use_data(pst1_energy, overwrite = TRUE)

# water series
pst1_water <- meter_df(interval = int,
                       water = set_units(rnorm(12), kW*h))

devtools::use_data(pst1_water, overwrite = TRUE)
