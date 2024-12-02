library("lubridate")

context("sun_angles")

test_that("sun_angles_24h", {
  #  test.path <- tempfile()
  test.path <- "./data/sun-angles-test-value"

  testthat::expect_known_value(
    sun_angles(time = ymd_hms("2012-10-22 12:00:00", tz = "UTC") + hours(0:24),
               geocode = data.frame(lon = 0, lat = c(89, 60, 45, 30, 0),
                                    address = "test"),
               use.refraction = FALSE),
    file = test.path
  )
}
)

test_that("sun_angles_geocode_vectorized", {
  sun.angles <-
    sun_angles(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
               geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
               use.refraction = FALSE)
  expect_equal(sun.angles$longitude, c(0, 0, 0, 0))
  expect_equal(sun.angles$latitude, c(-41, -3, 3, 41))
  expect_true(all(abs(sun.angles$azimuth -
                        c(359.0886, 180.7880, 180.6180, 180.3050)) < 1e-4))
  expect_equal(sun_azimuth(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                           geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                           use.refraction = FALSE),
               sun.angles$azimuth)

    expect_true(all(abs(sun.angles$elevation -
                        c(72.43013, 69.56602, 63.56646, 25.56747)) < 1e-4))
  expect_equal(sun_elevation(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                           geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                           use.refraction = FALSE),
               sun.angles$elevation)

  expect_equal(sun_zenith_angle(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                                use.refraction = FALSE),
               90 - sun.angles$elevation)

  expect_true(all(abs(sun.angles$distance -
                        c(0.9836573, 0.9836573, 0.9836573, 0.9836573)) < 1e-6))
  expect_equal(distance_to_sun(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                               geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                               use.refraction = FALSE),
               sun.angles$distance)

  sun.angles <-
    sun_angles(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
               geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
               use.refraction = TRUE)
  expect_equal(sun.angles$longitude, c(0, 0, 0, 0))
  expect_equal(sun.angles$latitude, c(-41, -3, 3, 41))
  expect_true(all(abs(sun.angles$azimuth -
                        c(359.0886, 180.7880, 180.6180, 180.3050)) < 1e-4))
  expect_true(all(abs(sun.angles$elevation -
                        c(72.43523, 69.57203, 63.57448, 25.60103)) < 1e-4))
  expect_true(all(abs(sun.angles$distance -
                        c(0.9836573, 0.9836573, 0.9836573, 0.9836573)) < 1e-6))
}
)

test_that("sun_angles_times_vectorized", {
  sun.angles <-
    sun_angles(ymd_hms("2012-10-22 12:00:00", tz = "UTC") + weeks(0:3),
               geocode = data.frame(lon = 0, lat = 41),
               use.refraction = FALSE)
  expect_equal(sun.angles$longitude, c(0, 0, 0, 0))
  expect_equal(sun.angles$latitude, c(41, 41, 41, 41))
  expect_true(all(abs(sun.angles$azimuth -
                        c(184.8340, 184.8604, 184.7117, 184.3951)) < 1e-4))
  expect_true(all(abs(sun.angles$elevation -
                        c(37.58375, 35.19321, 32.98920, 31.01003)) < 1e-4))
  expect_true(all(abs(sun.angles$distance -
                        c(0.9950469, 0.9931557, 0.9913616, 0.9896918)) < 1e-6))

  sun.angles <-
    sun_angles(ymd_hms("2012-10-22 12:00:00", tz = "UTC") + weeks(0:3),
               geocode = data.frame(lon = 0, lat = 41),
               use.refraction = TRUE)
  expect_equal(sun.angles$longitude, c(0, 0, 0, 0))
  expect_equal(sun.angles$latitude, c(41, 41, 41, 41))
  expect_true(all(abs(sun.angles$azimuth -
                        c(184.8340, 184.8604, 184.7117, 184.3951)) < 1e-4))
  expect_true(all(abs(sun.angles$elevation -
                        c(37.60468, 35.21603, 33.01399, 31.03679)) < 1e-4))
  expect_true(all(abs(sun.angles$distance -
                        c(0.9950469, 0.9931557, 0.9913616, 0.9896918)) < 1e-6))

  sun.angles <-
    sun_angles(ymd_hms("2012-10-22 16:30:00", tz = "UTC") + weeks(0:3),
               geocode = data.frame(lon = 0, lat = 41),
               use.refraction = FALSE)
  expect_equal(sun.angles$longitude, c(0, 0, 0, 0))
  expect_equal(sun.angles$latitude, c(41, 41, 41, 41))
  expect_true(all(abs(sun.angles$azimuth -
                        c(249.1641, 247.5674, 245.9957, 244.4684)) < 1e-4))
  expect_true(all(abs(sun.angles$elevation -
                        c(6.123102, 4.343891, 2.798342, 1.519157)) < 1e-4))
  expect_true(all(abs(sun.angles$distance -
                        c(0.9949952, 0.9931063, 0.9913151, 0.989649)) < 1e-6))

  sun.angles <-
    sun_angles(ymd_hms("2012-10-22 16:30:00", tz = "UTC") + weeks(0:3),
               geocode = data.frame(lon = 0, lat = 41),
               use.refraction = TRUE)
  expect_equal(sun.angles$longitude, c(0, 0, 0, 0))
  expect_equal(sun.angles$latitude, c(41, 41, 41, 41))
  expect_true(all(abs(sun.angles$azimuth -
                        c(249.1641, 247.5674, 245.9957, 244.4684)) < 1e-4))
  expect_true(all(abs(sun.angles$elevation -
                        c(6.259474, 4.521639, 3.036655, 1.837319)) < 1e-4))
  expect_true(all(abs(sun.angles$distance -
                        c(0.9949952, 0.9931063, 0.9913151, 0.989649)) < 1e-6))
}
)

