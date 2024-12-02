library("lubridate")

context("utilities")

test_that("tz_time_diff works correctly for dates and datetimes", {

  expect_equal(tz_time_diff(ymd("2020-06-01"), "Europe/Helsinki", "Europe/Helsinki"),
               0)

  expect_equal(tz_time_diff(ymd("2020-06-01"), "Europe/Helsinki"),
               3)

  expect_equal(tz_time_diff(ymd_hms("2020-06-01 01:15:00"), "UTC", "Europe/Helsinki"),
               -tz_time_diff(ymd_hms("2020-06-01 01:15:00"), "Europe/Helsinki", "UTC"))
  }
)

test_that("time of day works correctly for dates and datetimes", {

  expect_equal(as_tod(ymd("2020-06-01")), 0)
  expect_equal(as_tod(ymd_hms("2020-06-01 12:30:00")), 12.5) #hours
  expect_equal(as_tod(ymd_hms("2020-06-01 12:30:00"), unit.out = "hours"), 12.5)
  expect_equal(as_tod(ymd_hms("2020-06-01 12:30:00"), unit.out = "minutes"), 12.5 * 60)
  expect_equal(as_tod(ymd_hms("2020-06-01 12:30:00"), unit.out = "seconds"), 12.5 * 60 * 60)
  expect_error(as_tod(ymd_hms("2020-06-01 12:30:00"), unit.out = "bad_argument"))
  expect_error(as_tod(ymd_hms("2020-06-01 12:30:00"), unit.out = 123))

  expect_equal(capture.output(print(as_tod(ymd_hms("2020-06-01 12:30:00", tz = "UTC"),
                                                unit = "tod_time"))),
               "[1] \"12:30:00\"")
  expect_equal(capture.output(format(as_tod(ymd_hms("2020-06-01 12:30:00", tz = "UTC"),
                                           unit = "tod_time"))),
               "[1] \"12:30:00\"")

}
)

test_that("solar time works correctly for dates and datetimes", {

  expect_equal(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                          geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                          unit = "days"),
               rep(12.01999 / 24, times = 4),
               tolerance = 1e-5)
  expect_equal(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                          geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                          unit = "hours"),
               rep(12.01999, times = 4),
               tolerance = 1e-5)
  expect_equal(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                          geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                          unit = "minutes"),
               rep(12.01999 * 60, times = 4),
               tolerance = 1e-5)
  expect_equal(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                          geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                          unit = "seconds"),
               rep(12.01999 * 60 * 60, times = 4),
               tolerance = 1e-5)
  expect_equal(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                          geocode = data.frame(lon = c(30, 15, 0, -15, -30), lat = 60),
                          unit = "hours"),
               10.01999 + 4:0,
               tolerance = 1e-5)
  expect_true(is.solar_time(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                       geocode = data.frame(lon = c(30, 15, 0, -15, -30), lat = 60),
                                       unit = "time")))
  expect_true(is.solar_date(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                       geocode = data.frame(lon = c(30, 15, 0, -15, -30), lat = 60),
                                       unit = "datetime")))
  expect_true(is.solar_date(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                       geocode = data.frame(lon = c(30, 15, 0, -15, -30), lat = 60),
                                       unit = "date")))


  expect_equal(capture.output(print(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                               geocode = data.frame(lon = 0, lat = 60),
                                               unit = "time"))),
               "[1] \"12:01:11\"")
  expect_equal(capture.output(print(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                               geocode = data.frame(lon = 0, lat = 60),
                                               unit = "datetime"))),
               "[1] \"2012-12-22 12:01:11 solar\"")
  expect_equal(capture.output(print(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                               geocode = data.frame(lon = 0, lat = 60),
                                               unit = "date"))),
               "[1] \"2012-12-22 12:01:11 solar\"")

  expect_equal(capture.output(format(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                               geocode = data.frame(lon = 0, lat = 60),
                                               unit = "time"))),
               "[1] \"12:01:11\"")
  expect_equal(capture.output(format(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                                geocode = data.frame(lon = 0, lat = 60),
                                                unit = "datetime"))),
               "[1] \"2012-12-22 12:01:11\"")
  expect_equal(capture.output(format(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                                geocode = data.frame(lon = 0, lat = 60),
                                                unit = "date"))),
               "[1] \"2012-12-22 12:01:11\"")

}
)

test_that("geocode validation works correctly for dates and datetimes", {

  expect_error(validate_geocode(1:2))
  expect_error(validate_geocode(data.frame(lon = 181, lat = 60)))
  expect_error(validate_geocode(data.frame(lon = -181, lat = 60)))
  expect_no_error(validate_geocode(data.frame(lon = 180, lat = 60)))
  expect_no_error(validate_geocode(data.frame(lon = -180, lat = 60)))
  expect_error(validate_geocode(data.frame(lon = 0, lat = 90)))
  expect_error(validate_geocode(data.frame(lon = 0, lat = -90)))
  expect_no_error(validate_geocode(data.frame(lon = 0, lat = 89.99)))
  expect_no_error(validate_geocode(data.frame(lon = 0, lat = -89.99)))

  expect_no_error(validate_geocode(NA))
  expect_equal(validate_geocode(NA), na_geocode())
  expect_true(is_valid_geocode(na_geocode()))
  expect_false(is_valid_geocode(NA))
  expect_false(is_valid_geocode(data.frame(lon = 700, lat = 60)))

  expect_equal(length_geocode(data.frame(lon = 0, lat = 60)), 1L)
  expect_equal(length_geocode(data.frame(lon = 0:4 * 10, lat = rep(60, 5))), 5L)
  expect_true(is.na(length_geocode("abc")))
  expect_true(is.na(length_geocode(1)))
  expect_equal(length_geocode(na_geocode()), 1L)
}
)

