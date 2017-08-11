
library(units)
library(tibble)
context("reconcile meter data frames")

set.seed(5)

ps1_energy <- meter_df(time = seq.Date(as.Date("2013-1-1"),
                                       by = "+1 month",
                                       length.out = 12),
                       electricity = set_units(rnorm(12), kW))

ps1_water <- meter_df(time = seq.Date(as.Date("2013-1-1"),
                                      by = "+1 month",
                                      length.out = 12),
                      water = set_units(rnorm(12), parse_unit("Mgallon day-1")))

# merge point timestamp meter_df
test_that("reconciling meter_df with all point timestamps merges them", {
  expect_error(reconcile(ps1_energy, ps1_water), NA)
  expect_identical(dim(reconcile(ps1_energy, ps1_water)), c(12L, 3L))
  expect_identical(names(reconcile(ps1_energy, ps1_water)),
                   c("time", "electricity", "water"))
  expect_true(inherits(reconcile(ps1_energy, ps1_water), "meter_df"))
})
