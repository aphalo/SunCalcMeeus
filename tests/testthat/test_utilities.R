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
                          unit.out = "days"),
               rep(12.01999 / 24, times = 4),
               tolerance = 1e-5)
  expect_equal(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                          geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                          unit.out = "hours"),
               rep(12.01999, times = 4),
               tolerance = 1e-5)
  expect_equal(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                          geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                          unit.out = "minutes"),
               rep(12.01999 * 60, times = 4),
               tolerance = 1e-5)
  expect_equal(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                          geocode = data.frame(lon = 0, lat = c(-41, -3, 3, 41)),
                          unit.out = "seconds"),
               rep(12.01999 * 60 * 60, times = 4),
               tolerance = 1e-5)
  expect_equal(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                          geocode = data.frame(lon = c(30, 15, 0, -15, -30), lat = 60),
                          unit.out = "hours"),
               10.01999 + 4:0,
               tolerance = 1e-5)
  expect_true(is.solar_time(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                       geocode = data.frame(lon = c(30, 15, 0, -15, -30), lat = 60),
                                       unit.out = "time")))
  expect_true(is.solar_date(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                       geocode = data.frame(lon = c(30, 15, 0, -15, -30), lat = 60),
                                       unit.out = "datetime")))
  expect_true(is.solar_date(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                       geocode = data.frame(lon = c(30, 15, 0, -15, -30), lat = 60),
                                       unit.out = "date")))


  expect_equal(capture.output(print(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                               geocode = data.frame(lon = 0, lat = 60),
                                               unit.out = "time"))),
               "[1] \"12:01:11\"")
  expect_equal(capture.output(print(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                               geocode = data.frame(lon = 0, lat = 60),
                                               unit.out = "datetime"))),
               "[1] \"2012-12-22 12:01:11 solar\"")
  expect_equal(capture.output(print(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                               geocode = data.frame(lon = 0, lat = 60),
                                               unit.out = "date"))),
               "[1] \"2012-12-22 12:01:11 solar\"")

  expect_equal(capture.output(format(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                               geocode = data.frame(lon = 0, lat = 60),
                                               unit.out = "time"))),
               "[1] \"12:01:11\"")
  expect_equal(capture.output(format(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                                geocode = data.frame(lon = 0, lat = 60),
                                                unit.out = "datetime"))),
               "[1] \"2012-12-22 12:01:11\"")
  expect_equal(capture.output(format(solar_time(ymd_hms("2012-12-22 12:00:00", tz = "UTC"),
                                                geocode = data.frame(lon = 0, lat = 60),
                                                unit.out = "date"))),
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

  # validate_geocode() converts non-character addresses into character
  expect_no_error(validate_geocode(data.frame(lon = 180, lat = 60, address = "my address 1")))
  expect_no_error(validate_geocode(data.frame(lon = 180, lat = 60, address =  factor("my address 1"))))
  expect_equal(validate_geocode(data.frame(lon = 180, lat = 60, address = "my address 1")),
               validate_geocode(data.frame(lon = 180, lat = 60, address =  factor("my address 1"))))

  expect_no_error(validate_geocode(NA))
  expect_equal(validate_geocode(NA), na_geocode())
  expect_true(is_valid_geocode(na_geocode()))
  expect_true(is_valid_geocode(data.frame(lon = 10, lat = 60, address = "my address 1")))
  expect_warning(is_valid_geocode(data.frame(lon = 10, lat = 60, address = factor("my address 1"))))
  expect_false(suppressWarnings(
    is_valid_geocode(data.frame(lon = 10, lat = 60, address = factor("my address 1")))
    ))
  expect_false(is_valid_geocode(NA))
  expect_false(is_valid_geocode(data.frame(lon = 700, lat = 60)))
  expect_false(is_valid_geocode(data.frame(lon = 700, lat = 60, address = "my address 1")))

  two.geocodes <- list(data.frame(lon = 10, lat = 60, address = "my address 1"),
                       data.frame(lon = 30, lat = 20, address = "my address 2"))
  expect_true(is_valid_geocode(two.geocodes))
  two.geocodes <- list(data.frame(lon = 10, lat = 60, address = "my address 1"),
                       data.frame(lon = -300, lat = 20, address = "my address 2"))
  expect_false(is_valid_geocode(two.geocodes))

  expect_equal(length_geocode(data.frame(lon = 0, lat = 60)), 1L)
  expect_equal(length_geocode(data.frame(lon = 0:4 * 10, lat = rep(60, 5))), 5L)
  expect_true(is.na(length_geocode("abc")))
  expect_true(is.na(length_geocode(1)))
  expect_equal(length_geocode(na_geocode()), 1L)
}
)

test_that("split/bind for geocodes with address works correctly", {

  my.geocodes <-
    data.frame(spct.idx = c("A", "B", "C"),
               lon = c(0, 10, 15),
               lat = c(30, 60, 89),
               address = c("one", "two", "three")) |>
    validate_geocode()

  expect_silent(geo.list <- split_geocodes(my.geocodes))
  expect_named(geo.list, my.geocodes[["spct.idx"]])
  expect_true(length(geo.list) == nrow(my.geocodes))
  expect_equal(bind_geocodes(geo.list), my.geocodes)
  expect_error(split_geocodes(data.frame()))
  expect_error(bind_geocodes(list()))
  # silent pass-through
  expect_silent(split_geocodes(geo.list))
  expect_equal(split_geocodes(geo.list), geo.list)
  expect_silent(bind_geocodes(my.geocodes))
  expect_equal(bind_geocodes(my.geocodes), my.geocodes)
  # simplify
  expect_is(split_geocodes(my.geocodes[1, ]), "data.frame")
  expect_is(split_geocodes(my.geocodes[1, ],
                           simplify = TRUE), "data.frame")
  expect_is(split_geocodes(my.geocodes[1, ],
                           simplify = FALSE), "list")
}
)

test_that("split/bind for geocodes without address works correctly", {

  my.geocodes <-
    data.frame(spct.idx = c("A", "B", "C"),
               lon = c(0, 10, 15),
               lat = c(30, 60, 89)) |>
    validate_geocode()

  expect_silent(geo.list <- split_geocodes(my.geocodes))
  expect_named(geo.list, my.geocodes[["spct.idx"]])
  expect_true(length(geo.list) == nrow(my.geocodes))
  expect_equal(bind_geocodes(geo.list), my.geocodes)
  expect_error(split_geocodes(data.frame()))
  expect_error(bind_geocodes(list()))
  # silent pass-through
  expect_silent(split_geocodes(geo.list))
  expect_equal(split_geocodes(geo.list), geo.list)
  expect_silent(bind_geocodes(my.geocodes))
  expect_equal(bind_geocodes(my.geocodes), my.geocodes)
}
)

test_that("split/bind for geocodes with idx factor works correctly", {

  my.geocodes <-
    data.frame(ID = c("A", "B", "C"),
               lon = c(0, 10, 15),
               lat = c(30, 60, 89),
               address = c("one", "two", "three")) |>
    validate_geocode()

  expect_error(split_geocodes(my.geocodes))
  expect_silent(geo.list <- split_geocodes(my.geocodes,
                                           idx = "ID"))
  expect_named(geo.list, my.geocodes[["ID"]])
  expect_true(length(geo.list) == nrow(my.geocodes))
  expect_equal(bind_geocodes(geo.list, idx = "ID"),
               my.geocodes)
  expect_error(split_geocodes(data.frame()))
  expect_error(bind_geocodes(list()))
  # silent pass-through
  expect_silent(split_geocodes(geo.list))
  expect_equal(split_geocodes(geo.list), geo.list)
  expect_silent(bind_geocodes(my.geocodes))
  expect_equal(bind_geocodes(my.geocodes), my.geocodes)
}
)

test_that("split/bind for geocodes with extra column works correctly", {

  my.geocodes <-
    data.frame(spct.idx = c("A", "B", "C"),
               lon = c(0, 10, 15),
               lat = c(30, 60, 89),
               z = c("one", "two", "three")) |>
    validate_geocode()

  expect_silent(geo.list <- split_geocodes(my.geocodes))
  expect_named(geo.list, my.geocodes[["spct.idx"]])
  expect_true(length(geo.list) == nrow(my.geocodes))
  expect_equal(bind_geocodes(geo.list), my.geocodes)
  expect_error(split_geocodes(data.frame()))
  expect_error(bind_geocodes(list()))
  # silent pass-through
  expect_silent(split_geocodes(geo.list))
  expect_equal(split_geocodes(geo.list), geo.list)
  expect_silent(bind_geocodes(my.geocodes))
  expect_equal(bind_geocodes(my.geocodes), my.geocodes)
}
)
