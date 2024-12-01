#' Relative Air Mass (AM)
#'
#' Approximate relative air mass (AM) from the sun's position (sun elevation or
#' sun zenith angle) or from geographic and time coordinates.
#'
#' @param elevation.angle,zenith.angle numeric vector Angle in degrees for the
#'   sun position. An argument should be passed to one and only one of
#'   \code{elevation_angle} or \code{zenith_angle}.
#' @param occluded.value numeric Value to return when elevation angle is
#'   negative (sun below the horizon).
#'
#' @details Function \code{relative_AM()} implements equation (3) in Kasten and
#'   Young (1989). This equation is only an approximation to the tabulated
#'   values in the same paper. Returned values are rounded to three significant
#'   digits.
#'
#'   Function \code{relative_AM_geotime()} is a wrapper on \code{relative_AM()}
#'   that calls function \code{sun_elevation()} to obtain the position of the
#'   sun from the geographic and time coordinates.
#'
#' @note Although relative air mass is not defined when the sun is not visible,
#'   returning a value different from the default \code{NA} might be useful in
#'   some cases.
#'
#' @export
#'
#' @references
#' F. Kasten, A. T. Young (1989) Revised optical air mass tables and
#' approximation formula. Applied Optics, 28, 4735-. \doi{10.1364/ao.28.004735}.
#'
#' @examples
#'
#' relative_AM(elevation.angle = c(90, 60, 30, 1, -10))
#' relative_AM(elevation.angle = c(90, 60, 30, 1, -10),
#'             occluded.value = Inf)
#' relative_AM(zenith.angle = 0)
#'
#' library(lubridate)
#' relative_AM_geotime(ymd_hms("2014-09-23 12:00:00", tz = "UTC"),
#'                     geocode = data.frame(lat=60, lon=0))
#' relative_AM_geotime(ymd_hms("2014-09-23 12:00:00", tz = "UTC") + hours(0:12),
#'                     geocode = data.frame(lat=30, lon=0))
#'
relative_AM <- function(elevation.angle = NULL,
                        zenith.angle = NULL,
                        occluded.value = NA_real_) {
  stopifnot(xor(is.null(elevation.angle), is.null(zenith.angle)))
  if (is.null(elevation.angle)) {
    elevation.angle <- 90 - zenith.angle
  }
  stopifnot(all(elevation.angle >= -90 & elevation.angle <= 90))
  signif(
    ifelse(elevation.angle > 0,
           (sin(elevation.angle * pi / 180) + (0.1500 * (elevation.angle + 3.885)^-1.253))^-1,
           occluded.value[1]),
    3)
}

#' @rdname relative_AM
#'
#' @inheritParams sun_elevation
#'
#' @export
#'
relative_AM_geotime <-
  function(time = lubridate::now(tzone = "UTC"),
           tz = lubridate::tz(time),
           geocode = tibble::tibble(lon = 0, lat = 51.5, address = "Greenwich"),
           use.refraction = FALSE,
           occluded.value = NA_real_) {
    relative_AM(sun_elevation(time = time,
                              tz = tz,
                              geocode = geocode,
                              use.refraction = use.refraction),
                occluded.value = occluded.value)
  }
