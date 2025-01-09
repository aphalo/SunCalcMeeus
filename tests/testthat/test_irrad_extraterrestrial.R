library("lubridate")

context("Extraterrestrial irradiance")

test_that("Extraterrestrial irradiance value is as expected", {

  expect_equal(irrad_extraterrestrial(ymd_hm("2021-06-21 12:00", tz = "UTC")),
               1161.98252706)

  expect_equal(irrad_extraterrestrial(ymd_hm("2021-06-21 00:00", tz = "UTC")),
               0)

  expect_equal(irrad_extraterrestrial(ymd_hm("2021-06-21 12:00", tz = "UTC"),
                                      solar.constant = "NASA"),
               1161.98252706)

  expect_equal(irrad_extraterrestrial(ymd_hm("2021-06-21 00:00", tz = "UTC"),
                                      solar.constant = "NASA"),
               0)

  expect_equal(irrad_extraterrestrial(ymd_hm("2021-06-21 12:00", tz = "UTC"),
                                      solar.constant = "WMO"),
               1167.96331948)

  expect_equal(irrad_extraterrestrial(ymd_hm("2021-06-21 00:00", tz = "UTC"),
                                      solar.constant = "WMO"),
               0)

  expect_equal(irrad_extraterrestrial(ymd_hm("2021-06-21 12:00", tz = "UTC"),
                                      solar.constant = 1),
               0.854398916955)

  expect_equal(irrad_extraterrestrial(ymd_hm("2021-06-21 00:00", tz = "UTC"),
                                      solar.constant = 1),
               0)

  expect_error(irrad_extraterrestrial(ymd_hm("2021-06-21 00:00", tz = "UTC"),
                                      solar.constant = "bad"))

  expect_error(irrad_extraterrestrial(ymd_hm("2021-06-21 00:00", tz = "UTC"),
                                      solar.constant = -1))

}
)
