library("lubridate")

context("relative AM")

my.geocodes <-
  data.frame(lat = rep(c(0, 60, 80), each = 3),
            lon = rep(c(-90, 0, 90), times = 3),
            address = letters[1:9])

my.times <-
  ymd_hms("2024-06-21 06:00:00") + hours(0:12)

elevation.angles <- 9:0 * 10

test_that("relative_AM return value is as expected", {

  expect_equal(relative_AM(elevation.angle = elevation.angles),
               c(0.999, 1.010, 1.060, 1.150, 1.300, 1.550, 1.990, 2.900, 5.580, NA))

  expect_equal(relative_AM(zenith.angle = 90 - elevation.angles),
               c(0.999, 1.010, 1.060, 1.150, 1.300, 1.550, 1.990, 2.900, 5.580, NA))

  expect_error(relative_AM(elevation.angle = 90.001))
  expect_error(relative_AM(zenith.angle = -0.001))
  expect_no_error(relative_AM(elevation.angle = 90))
  expect_no_error(relative_AM(zenith.angle = 0))
  expect_no_error(relative_AM(elevation.angle = -1))
  expect_true(is.na(relative_AM(elevation.angle = -1)))
  expect_no_error(relative_AM(zenith.angle = 91))
  expect_true(is.na(relative_AM(zenith.angle = 91)))
  expect_no_error(relative_AM(elevation.angle = -0.6))
  expect_no_error(relative_AM(zenith.angle = 90.6))
}
)


test_that("relative_AMt return value is as expected", {

  expect_equal(relative_AMt(elevation.angle = elevation.angles),
               c(1.00, 1.02, 1.06, 1.15, 1.30, 1.55, 1.99,  2.90, 5.54, 31.70))

  expect_equal(relative_AMt(zenith.angle = 90 - elevation.angles),
               c(1.00, 1.02, 1.06, 1.15, 1.30, 1.55, 1.99,  2.90, 5.54, 31.70))

  expect_error(relative_AM(elevation.angle = 90.001))
  expect_error(relative_AM(zenith.angle = -0.001))
  expect_no_error(relative_AM(elevation.angle = 90))
  expect_no_error(relative_AM(zenith.angle = 0))
  expect_no_error(relative_AM(elevation.angle = -0.001))
  expect_true(is.na(relative_AM(elevation.angle = -0.001)))
  expect_no_error(relative_AM(zenith.angle = 90.001))
  expect_true(is.na(relative_AM(zenith.angle = 90.001)))
  expect_no_error(relative_AM(elevation.angle = -0.6))
  expect_no_error(relative_AM(zenith.angle = 90.6))

}
)

test_that("relative_AM_geotime return value is as expected", {

  expect_equal(relative_AM_geotime(time = my.times[1],
                                   tz = "UTC",
                                   geocode = my.geocodes),
               c(NA, 35.10, 1.09, NA, 2.90, 1.24, 4.21, 2.54,  1.81))

  expect_equal(relative_AM_geotime(time = my.times,
                                   tz = "UTC",
                                   geocode = my.geocodes[2, ]),
               c(35.10, 4.25, 2.20, 1.55, 1.26, 1.13, 1.09, 1.12, 1.25, 1.52, 2.14, 4.00, 27.40))
  }
)

test_that("relative_AMt_geotime return value is as expected", {

  expect_equal(relative_AMt_geotime(time = my.times[1],
                                   tz = "UTC",
                                   geocode = my.geocodes),
               c(NA, 36.60, 1.09, NA, 2.91, 1.24, 4.21, 2.54,  1.81))

  expect_equal(relative_AMt_geotime(time = my.times,
                                   tz = "UTC",
                                   geocode = my.geocodes[2, ]),
               c(36.60, 4.25, 2.20, 1.55, 1.26, 1.13, 1.09, 1.13, 1.25, 1.53, 2.14, 4.00, 27.50))
}
)
