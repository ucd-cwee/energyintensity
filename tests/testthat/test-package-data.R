library(energyintensity)
context("package sample data")

# included sample data
test_that("sample data is included", {

  data("pst1_water", "pst1_energy")

  expect_is(pst1_water, "meter_df")
  expect_is(pst1_energy, "meter_df")
})
