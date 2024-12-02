#' Times for sun positions
#'
#' Functions for calculating the timing of solar positions, given geographical
#' coordinates and dates. They can be also used to find the time for an
#' arbitrary solar elevation between 90 and -90 degrees by supplying "twilight"
#' angle(s) as argument.
#'
#' @param date "vector" of \code{POSIXct} times or\code{Date} objects, any valid
#'   TZ is allowed, default is current date at Greenwich matching the default
#'   for \code{geocode}.
#' @param tz character vector indicating time zone to be used in output and to
#'   interpret \code{Date} values passed as argument to \code{date}.
#' @param geocode data frame with one or more rows and variables lon and lat as
#'   numeric values (degrees). If present, address will be copied to the output.
#' @param twilight character string, one of "none", "rim", "refraction",
#'   "sunlight", "civil", "nautical", "astronomical", or a \code{numeric} vector
#'   of length one, or two, giving solar elevation angle(s) in degrees (negative
#'   if below the horizon).
#' @param unit.out character string, One of "datetime", "day", "hour", "minute",
#'   or "second".
#'
#' @return A tibble with variables day, tz, twilight.rise, twilight.set,
#'   longitude, latitude, address, sunrise, noon, sunset, daylength,
#'   nightlength or the corresponding individual vectors.
#'
#' @family astronomy related functions
#'
#' @details Twilight names are interpreted as follows. "none": solar elevation =
#'   0 degrees. "rim": upper rim of solar disk at the horizon or solar elevation
#'   = -0.53 / 2. "refraction": solar elevation = 0 degrees + refraction
#'   correction. "sunlight": upper rim of solar disk corrected for refraction,
#'   which is close to the value used by the online NOAA Solar Calculator.
#'   "civil": -6 degrees, "naval": -12 degrees, and "astronomical": -18 degrees.
#'   Unit names for output are as follows: "day", "hours", "minutes" and
#'   "seconds" times for sunrise and sunset are returned as times-of-day since
#'   midnight expressed in the chosen unit. "date" or "datetime" return the same
#'   times as datetime objects with TZ set (this is much slower than "hours").
#'   Day length and night length are returned as numeric values expressed in
#'   hours when `"datetime"' is passed as argument to \code{unit.out}. If
#'   twilight is a numeric vector of length two, the element with index 1 is
#'   used for sunrise and that with index 2 for sunset.
#'
#'   \code{is_daytime()} supports twilight specifications by name, a test
#'   like \code{sun_elevation() > 0} may be used directly for a numeric angle.
#'
#' @seealso \code{\link{sun_angles}}.
#'
#' @note Function \code{day_night()} is an implementation of Meeus equations as
#'   used in NOAAs on-line web calculator, which are very precise and valid for
#'   a very broad range of dates. For sunrise and sunset the times are affected
#'   by refraction in the atmosphere, which does in turn depend on weather
#'   conditions. The effect of refraction on the apparent position of the sun is
#'   only an estimate based on "typical" conditions. The more tangential to the
#'   horizon is the path of the sun, the larger the effect of refraction is on
#'   the times of visual occlusion of the sun behind the horizon---i.e. the
#'   largest timing errors occur at high latitudes. The computation is not
#'   defined for latitudes 90 and -90 degrees, i.e. at the poles.
#'
#'   There exists a different R implementation of the same algorithms called
#'   "AstroCalcPureR" available as function \code{astrocalc4r} in package
#'   'fishmethods'. Although the equations used are almost all the same, the
#'   function signatures and which values are returned differ. In particular,
#'   the implementation in 'photobiology' splits the calculation into two
#'   separate functions, one returning angles at given instants in time, and a
#'   separate one returning the timing of events for given dates. In
#'   'fishmethods' (= 1.11-0) there is a bug in function astrocalc4r() that
#'   affects sunrise and sunset times. The times returned by the functions in
#'   package 'photobiology' have been validated against the NOAA base
#'   implementation.
#'
#'   In the current implementation functions \code{sunrise_time},
#'   \code{noon_time}, \code{sunset_time}, \code{day_length},
#'   \code{night_length} and \code{is_daytime} are all wrappers
#'   on \code{day_night}, so if more than one quantity is needed it is
#'   preferable to directly call \code{day_night} and extract the different
#'   components from the returned list.
#'
#' @section Warning: Be aware that R's \code{Date} class does not save time zone
#'   metadata. This can lead to ambiguities in the current implementation
#'   based on time instants. The argument passed to \code{date} should be
#'   of class \code{POSIXct}, in other words an instant in time, from which
#'   the correct date will be computed based on the \code{tz} argument.
#'
#'   The time zone in which times passed to \code{date} as argument are
#'   expressed does not need to be the local one or match the geocode, however,
#'   the returned values will be in the same time zone as the input.
#'
#' @references
#' The primary source for the algorithm used is the book:
#' Meeus, J. (1998) Astronomical Algorithms, 2 ed., Willmann-Bell, Richmond,
#' VA, USA. ISBN 978-0943396613.
#'
#' A different implementation is available at
#' \url{https://github.com/NEFSC/READ-PDB-AstroCalc4R/} and in R paclage
#' 'fishmethods'. In 'fishmethods' (= 1.11-0) there is a bug in function
#' astrocalc4r() that affects sunrise and sunset times.
#'
#' An interactive web page using the same algorithms is available at
#' \url{https://gml.noaa.gov/grad/solcalc/}. There are small
#' differences in the returned times compared to our function that seem to be
#' related to the estimation of atmospheric refraction (about 0.1 degrees).
#'
#' @export
#' @examples
#' library(lubridate)
#'
#' my.geocode <- data.frame(lon = 24.93838,
#'                          lat = 60.16986,
#'                          address = "Helsinki, Finland")
#'
#' day_night(ymd("2015-05-30", tz = "EET"),
#'           geocode = my.geocode)
#' day_night(ymd("2015-05-30", tz = "EET") + days(1:10),
#'           geocode = my.geocode,
#'           twilight = "civil")
#' sunrise_time(ymd("2015-05-30", tz = "EET"),
#'              geocode = my.geocode)
#' noon_time(ymd("2015-05-30", tz = "EET"),
#'           geocode = my.geocode)
#' sunset_time(ymd("2015-05-30", tz = "EET"),
#'             geocode = my.geocode)
#' day_length(ymd("2015-05-30", tz = "EET"),
#'            geocode = my.geocode)
#' day_length(ymd("2015-05-30", tz = "EET"),
#'            geocode = my.geocode,
#'            unit.out = "day")
#' is_daytime(ymd("2015-05-30", tz = "EET") + hours(c(0, 6, 12, 18, 24)),
#'            geocode = my.geocode)
#' is_daytime(ymd_hms("2015-05-30 03:00:00", tz = "EET"),
#'            geocode = my.geocode)
#' is_daytime(ymd_hms("2015-05-30 00:00:00", tz = "UTC"),
#'            geocode = my.geocode)
#' is_daytime(ymd_hms("2015-05-30 03:00:00", tz = "EET"),
#'            geocode = my.geocode,
#'            twilight = "civil")
#' is_daytime(ymd_hms("2015-05-30 00:00:00", tz = "UTC"),
#'            geocode = my.geocode,
#'            twilight = "civil")
#'
day_night <- function(date = lubridate::now(tzone = "UTC"),
                      tz = ifelse(lubridate::is.Date(date),
                                  "UTC",
                                  lubridate::tz(date)),
                      geocode = tibble::tibble(lon = 0,
                                               lat = 51.5,
                                               address = "Greenwich"),
                      twilight = "none",
                      unit.out = "hours") {
  stopifnot(!anyNA(date)) # NAs could be propagated instead
  tz <- unique(tz)
  if (length(tz) > 1L) {
    tz <- tz[1]
    warning("'tz' is a heterogeneous vector, using only: ", tz)
  }
  geocode <- validate_geocode(geocode)
  if (any(lubridate::is.Date(date))) {
    date <- as.POSIXct(date, tz = tz, origin = lubridate::origin)
  }
  # as 'date' is not a Date but a time, we find the corresponding date in UTC
  # as calculations are done in UTC time
  date <- lubridate::with_tz(date, tzone = "UTC")
  date <- lubridate::floor_date(date, unit = "days")

  if (unit.out == "date") {
    unit.out <- "datetime"
  } else if (unit.out == "days") {
    unit.out <- "day"
  } else if (unit.out == "hours") {
    unit.out <- "hour"
  } else if (unit.out == "minutes") {
    unit.out <- "minute"
  } else if (unit.out == "seconds") {
    unit.out <- "second"
  }

  z <- list()
  for (i in seq_len(nrow(geocode))) {
    temp <- day_night_fast(date = date,
                           tz = tz, # used for returned times
                           geocode = dplyr::slice(geocode, i),
                           twilight = twilight,
                           unit.out = unit.out)
    z[[i]] <- temp
  }

  z <- do.call(rbind, z)

  # we use rbind instead of dplyr::bind_rows as the second drops the class attribute
  # for solartime.

  # assertion
  if (any(!is.na(z[["daylength"]]) & z[["daylength"]] < 0) ||
      any(!is.na(z[["nightlength"]]) & z[["nightlength"]] < 0)) {
    warning("Returned 'daylength/nightlength' value(s) off range")
  }
  attr(z, "unit.out") <- unit.out

  z
}

#' @rdname day_night
#'
day_night_fast <- function(date,
                           tz,
                           geocode,
                           twilight,
                           unit.out) {

  # Input validation done in day_night() before calling this function.
  # stopifnot(!anyNA(time))
  # date should be always a POSIXct object with tz set to "UTC" at hms all set to zero.
  # stopifnot(is.data.frame(geocode))
  # stopifnot(nrow(geocode == 1) && length(tz == 1))
  # We have a single geocode and all dates are expressed in the same time zone!
  # As date is a "vector" we can vectorize the whole calculation, and do the
  # expensive calculations only once.

  tz <- tz[1]

  multiplier <- switch(unit.out,
                       hour = 1,
                       minute = 60,
                       second = 3600,
                       day = 1/24,
                       1) # default

  # not vectorized, but possibly different angle for sunset and sunrise
  # twilight.angles is always of length 2
  twilight.angles <- twilight2angle(twilight)

  # geocode passed by day_night() has always one row
  if (!exists("address", geocode)) {
    geocode[["address"]] <- NA_character_
  }

  lon <- geocode[["lon"]]
  if (lon > 180 || lon < -180) {
    stop("Longitude is off-range.")
  }

  lat <- geocode[["lat"]]
  if (lat > 89.99 || lat < -89.99) {
    stop("Latitude is off-range.")
  }

  address <- geocode[["address"]]

  noon.of.date <- lubridate::with_tz(date, tzone = "UTC") + 43200 # faster

  cent <- julian_century(noon.of.date)

  tz.diff <- tz_time_diff(noon.of.date, tz.target = tz)

  sun.lon.mean <- geom_mean_lon_sun(cent)
  sun.anom.mean <- geom_mean_anom_sun(cent)
  eccent.earth <- eccent_earth_orbit(cent)
  delta <- sun_eq_of_ctr(cent, sun.anom.mean)

  sun.lon <- sun.lon.mean + delta
#  sun.anom <- sun.anom.mean + delta
#  sun.dist <- sun_rad_vector(eccent.earth, sun.anom)
  sun.app.lon <- sun_app_lon(cent, sun.lon)
  sun.ecliptic <- mean_obliq_eclip(cent)
  obliq.corr <- obliq_corr(cent, sun.ecliptic)
#  rt.ascen <- sun_rt_ascen(sun.app.lon, obliq.corr)
  sun.declin <- sun_decline(sun.app.lon, obliq.corr)
  var.y <- var_y(obliq.corr)
  eq.of.time <- eq_of_time(mean.lon = sun.lon.mean,
                           eccent.earth = eccent.earth,
                           anom.mean = sun.anom.mean,
                           var.y = var.y)

  solar.noon <- solar_noon(lon, eq.of.time)

  # We need to test for 24 h and 0 h days
  sun.noon.elevation <- elevation_angle(lat, 0, sun.declin)
  low.sunrise.sun <- sun.noon.elevation < max(twilight.angles[1])
  low.sunset.sun <- sun.noon.elevation < max(twilight.angles[2])

  sun.midnight.elevation <- elevation_angle(lat, 180, sun.declin)
  high.sunrise.sun <- sun.midnight.elevation > min(twilight.angles[1])
  high.sunset.sun <- sun.midnight.elevation > min(twilight.angles[2])

  # Vectorized
  day.start <-
    ifelse(!low.sunrise.sun & !high.sunrise.sun,
           # normal case
           sunrise(solar.noon,
                   ha_sunrise(lat, sun.declin, nag = -twilight.angles[1])),
           0
    )
  sunrise <- ifelse(day.start == 0, NA_real_, day.start)

  day.end <-
    ifelse(!low.sunset.sun & !high.sunset.sun,
           # normal case
           sunset(solar.noon,
                  ha_sunrise(lat, sun.declin, nag = -twilight.angles[2])),
           1
    )
  sunset <- ifelse(day.end == 1, NA_real_, day.end)

  daylength.hours <- ifelse(low.sunrise.sun & low.sunset.sun,
                            0,
                            (day.end - day.start) * 24)

  # we assemble the data frame for one geographic location
  # the slowest part of calculations are the calls to lubridate
  # so we try to minimize them according to output format.

  if (unit.out == "datetime") {
    sunrise.time <- date +
      lubridate::seconds(sunrise * 86400)
    noon.time    <- date +
      lubridate::seconds(solar.noon * 86400)
    sunset.time  <- date +
      lubridate::seconds(sunset * 86400)

    tibble::tibble(day           = date,
                   tz            = rep(!!tz, length(date)),
                   twilight.rise = rep(twilight.angles[1], length(date)),
                   twilight.set  = rep(twilight.angles[2], length(date)),
                   longitude     = rep(lon, length(date)),
                   latitude      = rep(lat, length(date)),
                   address       = rep(address, length(date)),
                   sunrise       = lubridate::with_tz(sunrise.time, tzone = !!tz),
                   noon          = lubridate::with_tz(noon.time, tzone = !!tz),
                   sunset        = lubridate::with_tz(sunset.time, tzone = !!tz),
                   daylength     = daylength.hours,
                   nightlength   = 24 - daylength.hours,
                   .name_repair  = "minimal"
    )
  } else if (unit.out %in% c("day", "hour", "minute", "second")) {
    sunrise.tod <- (sunrise * 24 + tz.diff) %% 24
    noon.tod <- (solar.noon * 24 + tz.diff) %% 24
    sunset.tod <- (sunset * 24 + tz.diff) %% 24

    tibble::tibble(day           = date,
                   tz            = rep(!!tz, length(date)),
                   twilight.rise = rep(twilight.angles[1], length(date)),
                   twilight.set  = rep(twilight.angles[2], length(date)),
                   longitude     = rep(lon, length(date)),
                   latitude      = rep(lat, length(date)),
                   address       = rep(address, length(date)),
                   sunrise       = sunrise.tod * multiplier,
                   noon          = noon.tod * multiplier,
                   sunset        = sunset.tod * multiplier,
                   daylength     = daylength.hours * multiplier,
                   nightlength   = (24 - daylength.hours) * multiplier,
                   .name_repair  = "minimal"
    )
  } else {
    stop("Unit out '", unit.out, "' not recognized")
  }
}

#' @rdname day_night
#'
#' @return \code{is_daytime()} returns a logical vector, with \code{TRUE} for
#'   day time and \code{FALSE} for night time.
#'
#' @export
#'
is_daytime <- function(date = lubridate::now(tzone = "UTC"),
                       tz = ifelse(lubridate::is.Date(date),
                                   "UTC",
                                   lubridate::tz(date)),
                       geocode = tibble::tibble(lon = 0,
                                                lat = 51.5,
                                                address = "Greenwich"),
                       twilight = "none",
                       unit.out = "hours") {
  if (!lubridate::is.POSIXct(date)) {
    warning("'date' must be a 'POSIXct' vector")
    return(rep(NA, length(date)))
  }
  z <- day_night(date = date,
                 tz = tz,
                 geocode = geocode,
                 twilight = twilight,
                 unit.out = "datetime")

  date > z[["sunrise"]] & date < z[["sunset"]]
}


#' @rdname day_night
#' @export
#' @return \code{noon_time}, \code{sunrise_time} and \code{sunset_time} return a
#'   vector of POSIXct times
#'
noon_time <- function(date = lubridate::now(tzone = "UTC"),
                      tz = lubridate::tz(date),
                      geocode = tibble::tibble(lon = 0,
                                               lat = 51.5,
                                               address = "Greenwich"),
                      twilight = "none",
                      unit.out = "datetime") {
  stopifnot(length(date) == 1 || nrow(geocode) == 1)
  day_night(date = date,
            tz = tz,
            geocode = geocode,
            twilight = twilight,
            unit.out = unit.out)[["noon"]]
}

#' @rdname day_night
#'
#' @export
#'
sunrise_time <- function(date = lubridate::now(tzone = "UTC"),
                         tz = lubridate::tz(date),
                         geocode = tibble::tibble(lon = 0,
                                                  lat = 51.5,
                                                  address = "Greenwich"),
                         twilight = "sunlight",
                         unit.out = "datetime") {
 #  stopifnot(length(date) == 1L || nrow(geocode) == 1L)
  day_night(date = date,
            tz = tz,
            geocode = geocode,
            twilight = twilight,
            unit.out = unit.out)[["sunrise"]]
}

#' @rdname day_night
#'
#' @export
#'
sunset_time <- function(date = lubridate::now(tzone = "UTC"),
                        tz = lubridate::tz(date),
                        geocode = tibble::tibble(lon = 0,
                                                 lat = 51.5,
                                                 address = "Greenwich"),
                        twilight = "sunlight",
                        unit.out = "datetime") {
  # stopifnot(length(date) == 1L || nrow(geocode) == 1L)
  day_night(date = date,
            tz = tz,
            geocode = geocode,
            twilight = twilight,
            unit.out = unit.out)[["sunset"]]
}

#' @rdname day_night
#'
#' @export
#' @return \code{day_length} and \code{night_length} return numeric a vector
#'   giving the length in hours
day_length <- function(date = lubridate::now(tzone = "UTC"),
                       tz = "UTC",
                       geocode = tibble::tibble(lon = 0,
                                                lat = 51.5,
                                                address = "Greenwich"),
                       twilight = "sunlight", unit.out = "hours") {
  # stopifnot(length(date) == 1L || nrow(geocode) == 1L)
  day_night(date = date,
            tz = tz,
            geocode = geocode,
            twilight = twilight,
            unit.out = unit.out)[["daylength"]]
}

#' @rdname day_night
#'
#' @export
#' @note \code{night_length} returns the length of night-time conditions in one
#'   day (00:00:00 to 23:59:59), rather than the length of the night between two
#'   consecutive days.
night_length <- function(date = lubridate::now(tzone = "UTC"),
                         tz = "UTC",
                         geocode = tibble::tibble(lon = 0,
                                                  lat = 51.5,
                                                  address = "Greenwich"),
                         twilight = "sunlight", unit.out = "hours") {
  # stopifnot(length(date) == 1L || nrow(geocode) == 1L)
  day_night(date = date,
              tz = tz,
              geocode = geocode,
              twilight = twilight,
              unit.out = unit.out)[["nightlength"]]
}
