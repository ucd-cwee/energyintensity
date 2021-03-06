
library(units)
library(tibble)
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


# tests -------------------------------------------------------------------

# meter_df from multiple vectors
test_that("point vectors are bound together", {
  mdf <- meter_df(date = dates,
                  energy = pt_values)
  mdf2 <- meter_df(interval = dates,
                   water = set_units(rnorm(12), parse_unit("Mgallon day-1")))

  expect_is(mdf, "meter_df")
  expect_is(mdf2, "meter_df")
})

test_that("interval vectors are bound together", {
  mdf <- meter_df(interval = int,
                  energy = int_values)

  expect_is(mdf, "meter_df")
})

# meter_df from data.frame
test_that("create meter_df from list", {
  mdf_pt <- meter_df(data.frame(date = dates,
                                energy = pt_values))
  mdf_int <- meter_df(data.frame(date = int,
                                 energy = int_values))
  expect_is(mdf_pt, "meter_df")
  expect_is(mdf_pt, "data.frame")
  expect_is(mdf_int, "meter_df")
  expect_is(mdf_int, "data.frame")
})

# meter_df from tbl_df
test_that("create meter_df from list", {
  mdf_pt <- meter_df(tibble(date = dates,
                            energy = pt_values))
  mdf_int <- meter_df(tibble(date = int,
                             energy = int_values))
  expect_is(mdf_pt, "meter_df")
  expect_is(mdf_pt, "tbl_df")
  expect_is(mdf_int, "meter_df")
  expect_is(mdf_int, "tbl_df")
})

# exactly one time field
test_that("exactly one time field", {
  f_no_time <- function() {
    meter_df(energy = pt_values)
  }
  f_two_time <- function() {
    meter_df(date = dates,
             date2 = dates,
             energy = pt_values)
  }

  expect_error(f_no_time(), "exactly one field should a have time/date stamp, or time/date-inteval values")
  expect_error(f_two_time(), "exactly one field should a have time/date stamp, or time/date-inteval values")
})

# no measurement field
test_that("no measurement field", {
  f_no_meas <- function() {
    meter_df(date = dates)
  }
  f_no_meas_units <- function() {
    meter_df(date = dates,
             energy = as.numeric(pt_values))
  }

  expect_error(f_no_meas(), "at least one field should have measured values with units set")
  expect_error(f_no_meas_units(), "at least one field should have measured values with units set")
})

# multiple measurement fields
test_that("multiple measurement fields", {
  f_mmeas <- function() {
    meter_df(date = dates,
             energy = pt_values,
             energy2 = pt_values + set_units(1, units(pt_values)),
             water = pt_values / 2)
  }
  f_mmeas_oth <- function() {
    meter_df(date = dates,
             energy = pt_values,
             water = pt_values / 2,
             other = 1:12)
  }

  expect_is(f_mmeas(), "meter_df")
  expect_is(f_mmeas_oth(), "meter_df")

  expect_equal(nrow(f_mmeas()), 12)
  expect_equal(nrow(f_mmeas_oth()), 12)

  expect_equal(length(f_mmeas()), 4)
  expect_equal(length(f_mmeas_oth()), 4)
})

# known units are accepted
test_that("known units are accepted", {
  f_bad_unit <- function() {
    meter_df(date = int,
             energy = set_units(rnorm(12), 'g')) # g: gram
  }

  f_good_unit <- function() {
    meter_df(date = dates,
             energy = set_units(rnorm(12), 'kW'))
  }

  expect_error(f_bad_unit(), "invalid units: g")
  expect_error(f_good_unit(), NA)
})

# ordinary fields are passed through
test_that("ordinary fields are passed through", {
  mdf_pt <- meter_df(data.frame(date = dates,
                                energy = pt_values,
                                other = 1:12))

  expect_true("other" %in% colnames(mdf_pt))
  expect_identical(mdf_pt$other, 1:12)
})

# time interval with rate measurment
test_that("time interval with rate measurment", {
  f_int_rate <- function() {
    meter_df(date = int,
             energy = pt_values)
  }

  expect_error(f_int_rate(), "time is interval, but measurement is rate")
})

# point timestamp with "volume" measurment
test_that("point timestamp with 'volume' measurment", {
  f_pt_vol <- function() {
    meter_df(date = dates,
             energy = int_values)
  }

  expect_error(f_pt_vol(), "time is point, but measurement is 'volume'")
})

# index ops
test_that("index operators work as expected", {
  mdf <- meter_df(date = dates,
                  energy = pt_values)

  expect_identical(mdf[1, ], meter_df(date = dates[1], energy = pt_values[1]))
  expect_identical(attr(mdf[1, ], "time_field"), "date")
  expect_identical(attr(mdf[1, ], "meas_fields"), "energy")
  expect_identical(nrow(mdf[2:5, ]), 4L)

  expect_identical(mdf[, "date"], dates)
  expect_identical(mdf$date, dates)
  expect_identical(mdf[['date']], dates)
})

# assignment ops
test_that("assignment operators work as expected", {
  mdf <- meter_df(date = dates,
                  energy = pt_values)

  mdf[['date2']] <- rev(dates)
  mdf$date3 <- dates

  expect_identical(mdf$date2, rev(dates))
  expect_identical(dim(mdf), c(12L, 4L))
  expect_identical(attr(mdf, "time_field"), "date")
  expect_identical(mdf$date3, dates)
})
