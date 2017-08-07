library(energyintensity)
context("meter df creation")

set.seed(5)

# interval sample data
st <- lubridate::ymd("2015-01-01")
int <- lubridate::interval(seq(st, st + months(11), by = "+1 month"),
                           seq(st + months(1), st + months(12), by = "+1 month"))
int_values <- units::set_units(rnorm(12), kW*h)

# point sample data
dates <- seq.Date(as.Date("2013-1-1"), by = "+1 month", length.out = 12)
pt_values <- units::set_units(rnorm(12), kW)

# meter_df from multiple vectors
test_that("interval vectors are bound together", {
  mdf <- meter_df(interval = int,
                  energy = int_values)

  expect_is(mdf, "meter_df")
})

test_that("point vectors are bound together", {
  mdf <- meter_df(date = dates,
                  energy = pt_values)

  expect_is(mdf, "meter_df")
})

# meter_df from list
# TODO

# meter_df from data.frame
# TODO

# meter_df from tbl_df
# TODO

# exactly one time field
# TODO

# no measurement field
# TODO

# multiple measurement fields
# TODO

# time interval with rate measurment
# TODO

# point timestamp with "volume" measurment
# TODO

# included sample data
test_that("package sample data", {

  data("pst1_water", "pst1_energy")

  expect_is(pst1_water, "meter_df")
  expect_is(pst1_energy, "meter_df")
})
