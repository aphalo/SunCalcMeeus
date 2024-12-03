## ----include=FALSE, echo=FALSE------------------------------------------------
knitr::opts_chunk$set(fig.width=8, fig.height=4)

## ----printing-spectra, eval=TRUE, include=FALSE-------------------------------
# library(tibble)
options(tibble.print_max = 6, tibble.print_min = 4)

## ----pkg-load, eval=TRUE, message = FALSE-------------------------------------
library(SunCalcMeeus)
library(lubridate)
# if installed, we use 'lutz' to lookup time zones from geocodes
eval_lutz <- requireNamespace("lutz", quietly = TRUE)
if (eval_lutz) {library(lutz)}

## -----------------------------------------------------------------------------
my.geocode <- data.frame(lat = 60.16, lon = 24.93, address = "Helsinki")

## -----------------------------------------------------------------------------
sun_angles(time = ymd_hms("2017-06-20 08:00:00", tz = "Europe/Helsinki"), 
           geocode = my.geocode)

## -----------------------------------------------------------------------------
sun_angles()

## -----------------------------------------------------------------------------
sun_angles(time = ymd_hms("2014-01-01 0:0:0", tz = "Europe/Helsinki") + hours(c(0, 6, 12)), 
           geocode = my.geocode)

## -----------------------------------------------------------------------------
my.times <- ymd_hms("2014-01-01 0:0:0", tz = "Europe/Helsinki") + hours(c(0, 6, 12))
sun_angles(time = my.times, geocode = my.geocode)

## -----------------------------------------------------------------------------
two.geocodes <- data.frame(lat = c(60.16, 65.02), 
                           lon = c(24.93, 25.47),
                           address = c("Helsinki", "Oulu"))
sun_angles(time = my.times, geocode = two.geocodes)

## -----------------------------------------------------------------------------
sun_elevation(time = my.times, geocode = my.geocode)

## -----------------------------------------------------------------------------
sun_zenith_angle(time = my.times, geocode = my.geocode)

## -----------------------------------------------------------------------------
sun_azimuth(time = my.times, geocode = my.geocode)

## -----------------------------------------------------------------------------
dates <- ymd("2015-03-01", tz = "Europe/Helsinki") + months(0:5)
dates

## -----------------------------------------------------------------------------
grep("Argentina", OlsonNames(), value = TRUE)

## -----------------------------------------------------------------------------
Sys.timezone()

## -----------------------------------------------------------------------------
# defaults to current UTC date and Greenwich, UK as location
sunrise_time()
sunrise_time(date = now(), tz = "Europe/Helsinki", geocode = my.geocode)
sunrise_time(date = now(tzone = "Europe/Helsinki"), geocode = my.geocode)
# time zone abbreviation not shown
print(sunrise_time(date = now("Europe/Helsinki"), 
                   geocode = my.geocode),
      usetz = FALSE)

## -----------------------------------------------------------------------------
## Using date times as POSIXct
sunrise_time(geocode = my.geocode)
sunrise_time(date = now("UTC"), geocode = my.geocode)
sunrise_time(date = now("UTC"), tz = "UTC", geocode = my.geocode)
sunrise_time(date = now("Europe/Helsinki"), geocode = my.geocode)
sunrise_time(date = now(""), tz = "Europe/Helsinki", geocode = my.geocode)
## Using Date
# correct always as time zones match
sunrise_time(today("Europe/Helsinki"), tz = "Europe/Helsinki", geocode = my.geocode)
# sometimes the value returned will be correct and sometimes off by 1 d at Helsinki
sunrise_time(today("Australia/Canberra"), tz = "Europe/Helsinki", geocode = my.geocode)

## -----------------------------------------------------------------------------
noon_time(now("UTC"), geocode = my.geocode)
noon_time(now("Europe/Helsinki"), geocode = my.geocode)

## -----------------------------------------------------------------------------
sunset_time(now("UTC"), geocode = my.geocode)
sunset_time(now("Europe/Helsinki"), geocode = my.geocode)
sunrise_time(now("Europe/Helsinki"), geocode = my.geocode)

## -----------------------------------------------------------------------------
day_length(dates, geocode = my.geocode)
night_length(dates, geocode = my.geocode)
day_length(dates, geocode = my.geocode, unit.out = "day")

## -----------------------------------------------------------------------------
sunrise_time(dates, tz = "America/Argentina/Buenos_Aires",
             geocode = data.frame(lat = -34.6, lon = -58.3))
noon_time(dates, tz = "America/Argentina/Buenos_Aires",
             geocode = data.frame(lat = -34.6, lon = -58.3))

## -----------------------------------------------------------------------------
sunrise_time(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = "none") # center of the sun disk
sunrise_time(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = "sunlight") # upper rim of the sun disk
sunrise_time(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = "rim") # lower rim of the sun disk
sunrise_time(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = "civil") # civil twilight = -6 degrees
sunrise_time(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = -10) # 10 degrees below the horizon
sunrise_time(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = +12) # 12 degrees above the horizon

## -----------------------------------------------------------------------------
day_length(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode)
day_length(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = 0)
day_length(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = "rim")
day_length(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = "civil")
day_length(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = -10)

## -----------------------------------------------------------------------------
day_length(ymd("2017-03-21", tz = "Europe/Helsinki"), 
           tz = "Europe/Helsinki", 
           geocode = my.geocode,
           twilight = c(0, 12))
night_length(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             twilight = c(0, 12))

## -----------------------------------------------------------------------------
sunrise_time(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode)
sunrise_time(ymd("2017-03-21", tz = "Europe/Helsinki"), 
             tz = "Europe/Helsinki", 
             geocode = my.geocode,
             unit.out = "hours")

## -----------------------------------------------------------------------------
day_length(dates, geocode = my.geocode, unit.out = "days")
night_length(dates, geocode = my.geocode, unit.out = "days")

## -----------------------------------------------------------------------------
day_night(dates[1:3], 
          geocode = my.geocode)

## -----------------------------------------------------------------------------
day_night(dates[1:3], 
          geocode = my.geocode, 
          unit.out = "days")

## -----------------------------------------------------------------------------
day_night(dates[1:3], 
          geocode = my.geocode, 
          unit.out = "datetime")

## -----------------------------------------------------------------------------
day_night(dates[1:3], 
          geocode = two.geocodes)

## -----------------------------------------------------------------------------
Paris.geo <- data.frame(lon = 2.352222, lat = 48.85661, address = "Paris")
Paris.time <- ymd_hms("2016-09-30 06:00:00", tz = "UTC")
solar_time(Paris.time, geocode = Paris.geo)
solar_time(Paris.time, geocode = Paris.geo, unit.out = "datetime")

## -----------------------------------------------------------------------------
my.solar.t <- solar_time(Paris.time, geocode = Paris.geo)
is.solar_time(my.solar.t)
is.numeric(my.solar.t)

## -----------------------------------------------------------------------------
my.solar.d <- solar_time(Paris.time, geocode = Paris.geo, unit.out = "datetime")
is.solar_date(my.solar.d)
is.timepoint(my.solar.d)

## -----------------------------------------------------------------------------
times <- now(tzone = "UTC") + hours(0:6)
times
as_tod(times)
as_tod(times, unit.out = "minutes")

## -----------------------------------------------------------------------------
relative_AM(33)

## -----------------------------------------------------------------------------
relative_AM(c(90, 60, 40, 20, 10, 5, 2, 1, 0.5))

## -----------------------------------------------------------------------------
relative_AMt(33)

## -----------------------------------------------------------------------------
relative_AMt(c(90, 60, 40, 20, 10, 5, 2, 1, 0.5))

## -----------------------------------------------------------------------------
january.times <- ymd_h("2020-01-01 12", tz = "Europe/Helsinki") + hours(-2:+2)
relative_AM_geotime(january.times, my.geocode, tz = "Europe/Helsinki")

## -----------------------------------------------------------------------------
relative_AMt_geotime(january.times, my.geocode, tz = "Europe/Helsinki")

