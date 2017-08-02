
# sample meter data from a pump station

library(units)
library(lubridate)

st <- ymd("2015-01-01")

pump_st_1 <- meter_df(interval = interval(seq(st, st + months(11), by = "+1 month"),
                                          seq(st + months(1), st + months(12), by = "+1 month")),
                      energy = set_units(rnorm(12), kW*h))

devtools::use_data(pump_st_1)
