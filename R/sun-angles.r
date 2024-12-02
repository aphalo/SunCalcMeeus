#' Sun angles
#'
#' Function \code{sun_angles()} returns the solar angles and Sun to Earth
#' relative distance for given times and locations using a very accurate
#' algorithm. Convenience functions \code{sun_azimuth()},
#' \code{sun_elevation()}, \code{sun_zenith_angle()} and
#' \code{distance_to_sun()} are wrappers on \code{sun_angles()} that return
#' individual vectors.
#'
#' @param time A "vector" of POSIXct Time, with any valid time zone (TZ) is
#'   allowed, default is current time.
#' @param tz character string indicating time zone to be used in output.
#' @param geocode data frame with variables lon and lat as numeric values
#'   (degrees), nrow > 1, allowed.
#' @param use.refraction logical Flag indicating whether to correct for
#'   fraction in the atmosphere.
#'
#' @return A \code{data.frame} with variables \code{time} (in same TZ as input),
#'   \code{TZ}, \code{solartime}, \code{longitude}, \code{latitude},
#'   \code{address}, \code{azimuth}, \code{elevation}, \code{declination},
#'   \code{eq.of.time}, \code{hour.angle}, and \code{distance}. If a data frame
#'   with multiple rows is passed to \code{geocode} and a vector of times longer
#'   than one is passed to \code{time}, sun position for all combinations of
#'   locations and times are returned by \code{sun_angles}. Angles are expressed
#'   in degrees, \code{solartime} is a vector of class \code{"solar.time"},
#'   \code{distance} is expressed in relative sun units.
#'
#' @family astronomy related functions
#'
#' @details This function is an implementation of Meeus equations as used in
#'   NOAAs on-line web calculator, which are precise and valid for a very broad
#'   range of dates (years -1000 to 3000 at least). The apparent solar
#'   elevations near sunrise and sunset are affected by refraction in the
#'   atmosphere, which does in turn depend on weather conditions. The effect of
#'   refraction on the apparent position of the sun is only an estimate based on
#'   "typical" conditions for the atmosphere. The computation is not defined for
#'   latitudes 90 and -90 degrees, i.e. exactly at the poles. The function is
#'   vectorized and in particular passing a vector of times for a single geocode
#'   enhances performance very much as the equation of time, the most time
#'   consuming step, is computed only once.
#'
#'   For improved performance, if more than one angle is needed it
#'   is preferable to directly call \code{sun_angles} instead of the wrapper
#'   functions as this avoids the unnecesary recalculation.
#'
#' @section Important!: Given an instant in time and a time zone, the date is
#'   computed from these, and may differ by one day to that at the location
#'   pointed by \code{geocode} at the same instant in time, unless the argument
#'   passed to \code{tz} matches the time zone at this location.
#'
#' @note There exists a different R implementation of the same algorithms called
#'   "AstroCalcPureR" available as function \code{astrocalc4r} in package
#'   'fishmethods'. Although the equations used are almost all the same, the
#'   function signatures and which values are returned differ. In particular,
#'   the present implementation splits the calculation into two separate
#'   functions, one returning angles at given instants in time, and a separate
#'   one returning the timing of events for given dates.
#'
#' @references
#' The primary source for the algorithm used is the book:
#' Meeus, J. (1998) Astronomical Algorithms, 2 ed., Willmann-Bell, Richmond,
#' VA, USA. ISBN 978-0943396613.
#'
#' A different implementation is available at
#' \url{https://github.com/NEFSC/READ-PDB-AstroCalc4R/}.
#'
#' An interactive web page using the same algorithms is available at
#' \url{https://gml.noaa.gov/grad/solcalc/}. There are small
#' differences in the returned times compared to our function that seem to be
#' related to the estimation of atmospheric refraction (about 0.1 degrees).
#'
#' @export
#'
#' @examples
#' library(lubridate)
#' sun_angles()
#' sun_azimuth()
#' sun_elevation()
#' sun_zenith_angle()
#' sun_angles(ymd_hms("2014-09-23 12:00:00"))
#' sun_angles(ymd_hms("2014-09-23 12:00:00"),
#'            geocode = data.frame(lat=60, lon=0))
#' sun_angles(ymd_hms("2014-09-23 12:00:00") + minutes((0:6) * 10))
#'
sun_angles <- function(time = lubridate::now(tzone = "UTC"),
                       tz = lubridate::tz(time),
                       geocode = tibble::tibble(lon = 0,
                                                lat = 51.5,
                                                address = "Greenwich"),
                       use.refraction = FALSE)
{
  geocode <- validate_geocode(geocode)
  stopifnot(lubridate::is.POSIXct(time))
  stopifnot(length(tz) == 1)

  z <- list(nrow(geocode))
  for (i in seq_len(nrow(geocode))) {
    z[[i]] <- sun_angles_fast(time = time,
                              tz = tz,
                              geocode = dplyr::slice(geocode, i),
                              use.refraction = use.refraction)
  }
  z <- dplyr::bind_rows(z)

  # assertion
  if (any(z[["elevation"]] < (-90)) || any(z[["elevation"]] > 90))
    warning("Returned 'elevation' value(s) off range")
  if (any(!is.na(z[["azimuth"]]) & (z[["azimuth"]] < -1e-10 |
                                    z[["azimuth"]] > (360 + 1e-10)))) {
    warning("Returned 'azimuth' values(s) off range")
  }
  z
}

#' @rdname sun_angles
#'
# Internal function, called by sun_angles()
#
sun_angles_fast <- function(time,
                            tz,
                            geocode,
                            use.refraction)
{
  # Input validation is done in sun_angles() before calling this function.
  # stopifnot(!anyNA(time))
  # stopifnot(is.data.frame(geocode))
  # stopifnot(nrow(geocode == 1) && length(tz == 1))
  # We have a single geocode and all times are expressed in the same time zone!
  # If time is a vector we can vectorize the whole calculation, and do the
  # expensive calculations only once.

  lon <- geocode[["lon"]]

  lat <- geocode[["lat"]]

  address <- geocode[["address"]]

  cent <- julian_century(time)

  sun.lon.mean <- geom_mean_lon_sun(cent)
  sun.anom.mean <- geom_mean_anom_sun(cent)
  eccent.earth <- eccent_earth_orbit(cent)
  delta <- sun_eq_of_ctr(cent, sun.anom.mean)

  sun.lon <- sun.lon.mean + delta
  sun.anom <- sun.anom.mean + delta
  sun.dist <- sun_rad_vector(eccent.earth, sun.anom)
  sun.app.lon <- sun_app_lon(cent, sun.lon)
  sun.ecliptic <- mean_obliq_eclip(cent)
  obliq.corr <- obliq_corr(cent, sun.ecliptic)
  #  rt.ascen <- sun_rt_ascen(sun.app.lon, obliq.corr)
  var.y <- var_y(obliq.corr)
  eq.of.time <- eq_of_time(mean.lon = sun.lon.mean,
                           eccent.earth = eccent.earth,
                           anom.mean = sun.anom.mean,
                           var.y = var.y)

  solar.time <- solar_tod(time, lat, lon, eq.of.time)
  hour.angle <- hour_angle(solar.time)
  sun.declin <- sun_decline(sun.app.lon, obliq.corr)
  zenith.angle <- zenith_angle(lat, hour.angle, sun.declin)
  elevation.angle <- 90 - zenith.angle
  if (use.refraction) {
    elevation.angle <-
      elevation.angle + atm_refraction_approx(elevation.angle)
  }
  azimuth.angle <- azimuth_angle(lat, hour.angle, zenith.angle, sun.declin)
  solar.time <- solar.time / 60 # hours
  solar.time <- solar.time  %% 24 # needed for DST
  class(solar.time) <- c("solar_time", class(solar.time))

  tibble::tibble(time = lubridate::with_tz(time, tz),
                 tz = rep(tz, length(time)),
                 solartime = solar.time,
                 longitude = rep(lon, length(time)),
                 latitude = rep(lat, length(time)),
                 address = rep(address, length(time)),
                 azimuth = azimuth.angle,
                 elevation = elevation.angle,
                 declination = sun.declin,
                 eq.of.time = eq.of.time,
                 hour.angle = hour.angle,
                 distance = sun.dist,
                 .name_repair = "minimal")
}

#' @rdname sun_angles
#'
#' @export
#'
sun_elevation <- function(time = lubridate::now(),
                          tz = lubridate::tz(time),
                          geocode = tibble::tibble(lon = 0,
                                                   lat = 51.5,
                                                   address = "Greenwich"),
                          use.refraction = FALSE)
{
  stopifnot(length(time) == 1 || nrow(geocode) == 1)
  sun_angles(time = time,
             tz = tz,
             geocode = geocode,
             use.refraction = use.refraction)[["elevation"]]
}

#' @rdname sun_angles
#'
#' @export
#'
sun_zenith_angle <- function(time = lubridate::now(),
                             tz = lubridate::tz(time),
                             geocode = tibble::tibble(lon = 0,
                                                      lat = 51.5,
                                                      address = "Greenwich"),
                             use.refraction = FALSE)
{
  stopifnot(length(time) == 1 || nrow(geocode) == 1)
  90 - sun_angles(time = time,
                  tz = tz,
                  geocode = geocode,
                  use.refraction = use.refraction)[["elevation"]]
}

#' @rdname sun_angles
#'
#' @export
#'
sun_azimuth <- function(time = lubridate::now(),
                        tz = lubridate::tz(time),
                        geocode = tibble::tibble(lon = 0,
                                                 lat = 51.5,
                                                 address = "Greenwich"),
                        use.refraction = FALSE)
{
  stopifnot(length(time) == 1 || nrow(geocode) == 1)
  sun_angles(time = time,
             tz = tz,
             geocode = geocode,
             use.refraction = use.refraction)[["azimuth"]]
}

#' @rdname sun_angles
#'
#' @export
#'
distance_to_sun <- function(time = lubridate::now(),
                            tz = lubridate::tz(time),
                            geocode = tibble::tibble(lon = 0,
                                                     lat = 51.5,
                                                     address = "Greenwich"),
                            use.refraction = FALSE)
{
  stopifnot(length(time) == 1 || nrow(geocode) == 1)
  sun_angles(time = time,
             tz = tz,
             geocode = geocode,
             use.refraction = use.refraction)[["distance"]]
}

