library("lubridate")

context("sun_times")

test_that("day_night handles dubious argument values", {

  my.geocode <- data.frame(lon = 24.93838,
                           lat = 60.16986,
                           address = "Helsinki, Finland")

  expect_is(day_night(ymd("2015-05-30", tz = "Europe/Helsinki"), geocode = my.geocode),
            "data.frame")

  expect_silent(day_night(ymd("2015-05-30"), geocode = my.geocode, tz = "Europe/Helsinki"))
  expect_silent(day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode))
  expect_silent(day_night(ymd("2015-05-30"), geocode = my.geocode, tz = ""))
  expect_silent(day_night(ymd("2015-05-30"), geocode = my.geocode, tz = c("UTC", "UTC")))
  expect_warning(day_night(ymd("2015-05-30"), geocode = my.geocode, tz = c("UTC", "")))
  expect_warning(day_night(ymd("2015-05-30"), geocode = my.geocode, tz = c("UTC", "GMT")))
  expect_equal(day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode,
                         unit.out = "minute"),
               day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode,
                         unit.out = "minutes"))
  expect_equal(day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode,
                         unit.out = "second"),
               day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode,
                         unit.out = "seconds"))
  expect_equal(day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode,
                         unit.out = "day"),
               day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode,
                         unit.out = "days"))
  expect_equal(day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode,
                         unit.out = "date"),
               day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode,
                         unit.out = "datetime"))
  geocode.no.address <- geocode.na.address <- my.geocode
  geocode.no.address[["address"]] <- NULL
  geocode.na.address[["address"]] <- NA_character_
  expect_equal(day_night(ymd("2015-05-30", tz = ""), geocode = geocode.na.address,
                         unit.out = "date"),
               day_night(ymd("2015-05-30", tz = ""), geocode = geocode.no.address,
                         unit.out = "datetime"))
  geocode.lon.off.range <- geocode.lat.off.range <- my.geocode
  geocode.lon.off.range[["lon"]] <- 181
  geocode.lat.off.range[["lat"]] <- 91
  expect_error(day_night(ymd("2015-05-30", tz = ""), geocode = geocode.lon.off.range))
  expect_error(day_night(ymd("2015-05-30", tz = ""), geocode = geocode.lat.off.range))
  expect_error(day_night(ymd("2015-05-30", tz = ""), geocode = my.geocode, unit.out = "bad"))
})

test_that("is_daytime() handles dubious argument values", {
  expect_warning(is_daytime(date = 1))
  expect_true(is.na(suppressWarnings(is_daytime(date = 1))))
  expect_equal(length(suppressWarnings(is_daytime(date = 1:5))), 5)
})

test_that("sunrise_time", {

  expect_equal(
    tz(sunrise_time()), "UTC"
  )
  expect_equal(
    tz(sunrise_time(geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"))), "UTC"
  )
  expect_equal(
    tz(sunrise_time(ymd("2016-04-17", tz = "UTC"),
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "UTC"
  )
  expect_equal(
    tz(sunrise_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "UTC"
  )
  expect_equal(
    tz(sunrise_time(ymd_hms("2016-04-17 12:00:20", tz = "UTC"), tz = "UTC",
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "UTC"
  )
  expect_equal(
    tz(sunrise_time(ymd_hms("2016-04-17 12:00:20", tz = "UTC"), tz = "Europe/Helsinki",
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "Europe/Helsinki"
  )
  expect_equal(
    as.duration(
      sunrise_time(ymd("2016-04-17", tz = "UTC"), tz = "Europe/Helsinki",
                   geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                        address = "Helsinki, Finland"),
                   twilight = "none") %--%
        sunrise_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                     geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                          address = "Helsinki, Finland"),
                     twilight = "none")), as.duration(seconds(0))
  )
  expect_lt(
    abs(as.numeric(sunrise_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                                geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                                     address = "Helsinki, Finland"),
                                twilight = "none") -
                     ymd_hms("2016-04-17 03:02:32", tz = "UTC"))), 1
  )
  expect_lt(
    abs(as.numeric(sunrise_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                                geocode = data.frame(lon = 25.46508, lat = 65.01209,
                                                     address = "Oulu, Finland"),
                                twilight = "none") -
                     ymd_hms("2016-04-17 02:41:40", tz = "UTC"))), 1
  )
  expect_lt(
    abs(as.numeric(sunrise_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                                geocode = data.frame(lon = 27.02853, lat = 69.90905,
                                                     adress = "Utsjoki, Finland"),
                                twilight = "none") -
                     ymd_hms("2016-04-17 02:06:34", tz = "UTC"))), 1
  )
  expect_lt(
    abs(as.numeric(sunrise_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                                geocode = data.frame(lon = -68.30295, lat = -54.80191,
                                                     address = "Ushuaia, Argentina"),
                                twilight = "none") -
                     ymd_hms("2016-04-17 11:34:59", tz = "UTC"))), 1
  )
  expect_lt(
    as.numeric(sunrise_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                            geocode = data.frame(lon = 23.67027, lat = 77.5536),
                            twilight = "none") -
                 ymd_hms("2016-04-17 00:28:16", tz = "UTC")), 1
  )
  expect_lt(
    as.numeric(sunrise_time(ymd("2016-04-21", tz = "UTC"), tz = "UTC",
                            geocode = data.frame(lon = 23.67027, lat = 77.5536),
                            twilight = "none") -
                 ymd_hms("2016-04-20 23:18:52", tz = "UTC")), 1
  )
  expect_true(
    is.na(sunrise_time(ymd("2016-04-23", tz = "UTC"), tz = "UTC",
                       geocode = data.frame(lon = 23.67027, lat = 77.5536),
                       twilight = "none"))
  )
})

test_that("noon_time", {

  expect_equal(
    tz(noon_time()), "UTC"
  )
  expect_equal(
    tz(noon_time(geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                      address = "Helsinki, Finland"))), "UTC"
  )
  expect_equal(
    tz(noon_time(ymd("2016-04-17", tz = "UTC"),
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "UTC"
  )
  expect_equal(
    tz(noon_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "UTC"
  )
  expect_equal(
    tz(noon_time(ymd_hms("2016-04-17 12:00:20", tz = "UTC"), tz = "UTC",
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "UTC"
  )
  expect_equal(
    tz(noon_time(ymd_hms("2016-04-17 12:00:20", tz = "UTC"), tz = "Europe/Helsinki",
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "Europe/Helsinki"
  )
  expect_equal(
    as.duration(
      noon_time(ymd("2016-04-17", tz = "UTC"), tz = "Europe/Helsinki",
                   geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                        address = "Helsinki, Finland"),
                   twilight = "none") %--%
        noon_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                     geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                          address = "Helsinki, Finland"),
                     twilight = "none")), as.duration(seconds(0))
  )
  expect_lt(
    abs(as.numeric(noon_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                                geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                                     address = "Helsinki, Finland"),
                                twilight = "none") -
                     ymd_hms("2016-04-17 10:19:42", tz = "UTC"))), 1
  )
  expect_lt(
    abs(as.numeric(noon_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                                geocode = data.frame(lon = 25.46508, lat = 65.01209,
                                                     address = "Oulu, Finland"),
                                twilight = "none") -
                     ymd_hms("2016-04-17 10::17:36", tz = "UTC"))), 1
  )
  expect_lt(
    abs(as.numeric(noon_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                                geocode = data.frame(lon = 27.02853, lat = 69.90905,
                                                     adress = "Utsjoki, Finland"),
                                twilight = "none") -
                     ymd_hms("2016-04-17 10:11:20", tz = "UTC"))), 1
  )
  expect_lt(
    abs(as.numeric(noon_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                                geocode = data.frame(lon = -68.30295, lat = -54.80191,
                                                     address = "Ushuaia, Argentina"),
                                twilight = "none") -
                     ymd_hms("2016-04-17 16:32:40", tz = "UTC"))), 1
  )
  expect_lt(
    as.numeric(noon_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                            geocode = data.frame(lon = 23.67027, lat = 77.5536),
                            twilight = "none") -
                 ymd_hms("2016-04-17 10:24:46", tz = "UTC")), 1
  )
  expect_lt(
    as.numeric(noon_time(ymd("2016-04-21", tz = "UTC"), tz = "UTC",
                            geocode = data.frame(lon = 23.67027, lat = 77.5536),
                            twilight = "none") -
                 ymd_hms("2016-04-20 10:23:56", tz = "UTC")), seconds(1.5)
               )
})

test_that("sunset_time", {
  expect_equal(
    tz(sunset_time()), "UTC"
  )
  expect_equal(
    tz(sunset_time(geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                      address = "Helsinki, Finland"))), "UTC"
  )
  expect_equal(
    tz(sunset_time(ymd("2016-04-17", tz = "UTC"),
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "UTC"
  )
  expect_equal(
    tz(sunset_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "UTC"
  )
  expect_equal(
    tz(sunset_time(ymd_hms("2016-04-17 12:00:20", tz = "UTC"), tz = "UTC",
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "UTC"
  )
  expect_equal(
    tz(sunset_time(ymd_hms("2016-04-17 12:00:20", tz = "UTC"), tz = "Europe/Helsinki",
                    geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                         address = "Helsinki, Finland"),
                    twilight = "none")), "Europe/Helsinki"
  )
  expect_equal(
    as.duration(
      sunset_time(ymd("2016-04-17", tz = "UTC"), tz = "Europe/Helsinki",
                   geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                        address = "Helsinki, Finland"),
                   twilight = "none") %--%
        sunset_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                     geocode = data.frame(lon = 24.93838, lat = 60.16986,
                                          address = "Helsinki, Finland"),
                     twilight = "none")), as.duration(seconds(0))
  )
  expect_lt(
    as.numeric(sunset_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                           geocode = data.frame(lon = 24.93838, lat = 60.16986),
                           #                            geocode = geocode("Helsinki, Finland"),
                           twilight = "none") -
                 ymd_hms("2016-04-17 17:36:52", tz = "UTC")), 1
  )
  expect_lt(
    as.numeric(sunset_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                           geocode = data.frame(lon = 25.46508, lat = 65.01209),
                           #                           geocode = geocode("Oulu, Finland"),
                           twilight = "none") -
                 ymd_hms("2016-04-17 17:53:31", tz = "UTC")), 1
  )
  expect_lt(
    as.numeric(sunset_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                           geocode = data.frame(lon = 27.02853, lat = 69.90905),
                           #                          geocode = geocode("Utsjoki, Finland"),
                           twilight = "none") -
                 ymd_hms("2016-04-17 18:16:06", tz = "UTC")), 1
  )
})

test_that("sunrise_time_vectorized", {
  expect_equal(
    length(sunrise_time(ymd("2016-04-17", tz = "UTC") + days(0:5),
                        geocode = data.frame(lon = 24.93838, lat = 60.16986),
                        #                       geocode = geocode("Helsinki, Finland"),
                        twilight = "none")), 6)
  expect_equal(
    sunrise_time(ymd("2016-04-17", tz = "UTC") + days(0:5), tz = "UTC",
                 geocode = data.frame(lon = 24.93838, lat = 60.16986),
                 #                       geocode = geocode("Helsinki, Finland"),
                 twilight = "none")[1],
    sunrise_time(ymd("2016-04-17", tz = "UTC"),
                 geocode = data.frame(lon = 24.93838, lat = 60.16986), tz = "UTC",
                 #                       geocode = geocode("Helsinki, Finland"),
                 twilight = "none"))
  expect_equal(
    length(sunrise_time(ymd("2016-04-20", tz = "UTC") + days(0:5),
                        geocode = data.frame(lon = 23.67027, lat = 77.5536),
                        #                           geocode = geocode("Ushuaia, Argentina"),
                        twilight = "none")), 6)
  expect_equal(
    sunrise_time(ymd("2016-04-17", tz = "UTC") + days(0:5), tz = "UTC",
                 geocode = data.frame(lon = 23.67027, lat = 77.5536),
                 #                           geocode = geocode("Ushuaia, Argentina"),
                 twilight = "none")[1],
    sunrise_time(ymd("2016-04-17", tz = "UTC"), tz = "UTC",
                 geocode = data.frame(lon = 23.67027, lat = 77.5536),
                 #                           geocode = geocode("Ushuaia, Argentina"),
                 twilight = "none"))
  expect_equal(
    sunrise_time(ymd("2016-04-20", tz = "UTC") + days(0:5), tz = "UTC",
                 geocode = data.frame(lon = 23.67027, lat = 77.5536),
                 #                           geocode = geocode("Ushuaia, Argentina"),
                 twilight = "none")[3],
    sunrise_time(ymd("2016-04-22", tz = "UTC"), tz = "UTC",
                 geocode = data.frame(lon = 23.67027, lat = 77.5536),
                 #                           geocode = geocode("Ushuaia, Argentina"),
                 twilight = "none"))
})

test_that("sunset_time_vectorized", {
  expect_equal(
    length(sunset_time(ymd("2016-04-17", tz = "UTC") + days(0:5), tz = "UTC",
                       geocode = data.frame(lon = 24.93838, lat = 60.16986),
                       #                     geocode = geocode("Helsinki, Finland"),
                       twilight = "none")), 6)
})

test_that("daylength", {

  expect_equal(day_length(now(tzone = "UTC")), day_length(today(tzone = "UTC"))) # is conversion o.k.?
  expect_equal(day_length(ymd("2014-12-21"),
                          geocode = data.frame(lat = 85, lon = 0)),
               0)
  expect_equal(day_length(ymd("2014-06-21"),
                          geocode = data.frame(lat = 85, lon = 0)),
               24)
  expect_equal(night_length(ymd("2014-12-21"),
                            geocode = data.frame(lat = 85, lon = 0)),
               24)
  expect_equal(night_length(ymd("2014-06-21"),
                            geocode = data.frame(lat = 85, lon = 0)),
               0)
  expect_equal(round(day_length(ymd("2014-12-21"),
                                geocode = data.frame(lat = 0, lon = 0)), 3),
               12.121)
  expect_equal(round(day_length(ymd("2014-06-21"),
                                geocode = data.frame(lat = 0, lon = 0)), 3),
               12.121)
  expect_equal(round(night_length(ymd("2014-12-21"),
                                  geocode = data.frame(lat = 0, lon = 0)), 3),
               11.879)
  expect_equal(round(night_length(ymd("2014-06-21"),
                                  geocode = data.frame(lat = 0, lon = 0)), 3),
               11.879)

  expect_equal(round(day_length(ymd("2014-03-20"),
                                geocode = data.frame(lat = 45, lon = 0)), 3),
               12.147)
  expect_equal(round(day_length(ymd("2014-09-22"),
                                geocode = data.frame(lat = 45, lon = 0)), 3),
               12.188)
  expect_equal(round(night_length(ymd("2014-03-20"),
                                  geocode = data.frame(lat = 45, lon = 0)), 3),
               11.853)
  expect_equal(round(night_length(ymd("2014-09-22"),
                                  geocode = data.frame(lat = 45, lon = 0)), 3),
               11.812)

  expect_gt(round(day_length(ymd("2014-03-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = "civil"), 2), 12)
  expect_gt(round(day_length(ymd("2014-09-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = "civil"), 2), 12)
  expect_lt(round(night_length(ymd("2014-03-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = "civil"), 2), 12)
  expect_lt(round(night_length(ymd("2014-09-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = "civil"), 2), 12)

  expect_gt(round(day_length(ymd("2014-03-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = "nautical"), 2), 12)
  expect_gt(round(day_length(ymd("2014-09-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = "nautical"), 2), 12)
  expect_lt(round(night_length(ymd("2014-03-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = "nautical"), 2), 12)
  expect_lt(round(night_length(ymd("2014-09-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = "nautical"), 2), 12)

  expect_gt(round(day_length(ymd("2014-03-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = "astronomical"), 2), 12)
  expect_gt(round(day_length(ymd("2014-09-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = "astronomical"), 2), 12)
  expect_lt(round(night_length(ymd("2014-03-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = "astronomical"), 2), 12)
  expect_lt(round(night_length(ymd("2014-09-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = "astronomical"), 2), 12)

  expect_gt(round(day_length(ymd("2014-03-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = -1), 2), 12)
  expect_gt(round(day_length(ymd("2014-09-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = -1), 2), 12)
  expect_lt(round(night_length(ymd("2014-03-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = -1), 2), 12)
  expect_lt(round(night_length(ymd("2014-09-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = -1), 2), 12)

  expect_lt(round(day_length(ymd("2014-03-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = +1), 2), 12)
  expect_lt(round(day_length(ymd("2014-09-21"),
                             geocode = data.frame(lat = 45, lon = 0),
                             twilight = +1), 2), 12)
  expect_gt(round(night_length(ymd("2014-03-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = +1), 2), 12)
  expect_gt(round(night_length(ymd("2014-09-21"),
                               geocode = data.frame(lat = 45, lon = 0),
                               twilight = +1), 2), 12)

  expect_equal(night_length(ymd("2014-09-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = "none"),
               night_length(ymd("2014-09-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = 0))
  expect_equal(night_length(ymd("2014-09-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = "civil"),
               night_length(ymd("2014-09-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = -6))
  expect_equal(night_length(ymd("2014-09-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = "civil"),
               night_length(ymd("2014-09-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = c(-6, -6)))

  testthat::expect_true(is_daytime(ymd_hm("2014-03-21 12:00"),
                                   geocode = data.frame(lat = 45, lon = 0)))
  testthat::expect_false(is_daytime(ymd_hm("2014-03-21 23:59"),
                                    geocode = data.frame(lat = 45, lon = 0)))

  expect_warning(day_length(ymd("2014-03-21"),
                          geocode = data.frame(lat = 45, lon = 0),
                          twilight = rep("none", 2)))
  expect_warning(day_length(ymd("2014-03-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = "bad"))
  expect_warning(day_length(ymd("2014-03-21"),
                          geocode = data.frame(lat = 45, lon = 0),
                          twilight = +91))
  expect_warning(day_length(ymd("2014-03-21"),
                          geocode = data.frame(lat = 45, lon = 0),
                          twilight = -91))
  expect_warning(day_length(ymd("2014-03-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = NA))
  expect_warning(day_length(ymd("2014-03-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = NULL))
  expect_error(day_length(ymd("2014-03-21"),
                            geocode = data.frame(lat = 45, lon = 0),
                            twilight = rep(0, 3)))
  expect_warning(day_length(ymd("2014-03-21"),
                          geocode = data.frame(lat = 45, lon = 0),
                          twilight = numeric()))
  expect_warning(day_length(ymd("2014-03-21"),
                          geocode = data.frame(lat = 45, lon = 0),
                          twilight = character()))

})

