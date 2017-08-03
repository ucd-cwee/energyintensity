library(energyintensity)
context("meter df creation")

test_that("meter_df constructor", {

  # sample data
  st <- lubridate::ymd("2015-01-01")
  int <- lubridate::interval(seq(st, st + months(11), by = "+1 month"),
                             seq(st + months(1), st + months(12), by = "+1 month"))
  mdf <- meter_df(interval = int,
                  energy = units::set_units(rnorm(12), kW*h))

  expect_is(mdf, "meter_df")
})

test_that("package sample data", {

  data("pst1_water", "pst1_energy")

  expect_is(pst1_water, "meter_df")
  expect_is(pst1_energy, "meter_df")
})
