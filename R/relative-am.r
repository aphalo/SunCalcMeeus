#' Relative Air Mass (AM)
#'
#' Approximate relative air mass (AM) computed from the sun's apparent or true
#' position (sun elevation or sun zenith angle) or from geographic and time
#' coordinates.
#'
#' @param elevation.angle,zenith.angle numeric vector Angle in degrees for the
#'   sun position. An argument should be passed to one and only one of
#'   \code{elevation_angle} or \code{zenith_angle}.
#' @param occluded.value numeric Value to return when elevation angle is
#'   negative (sun below the horizon).
#'
#' @details Function \code{relative_AM()} implements equation (3) in Kasten and
#'   Young (1989). This equation is only an approximation to the tabulated
#'   values in the same paper and based on the apparent position of the sun as
#'   observed from Earth surface. \code{relative_AMt()} implements equation (5)
#'   in Young (1994). This equation is only an approximation to the tabulated
#'   values based on the true or astronomical position of the sun.
#'
#'   In both cases returned values are rounded to three significant digits.
#'
#'   Function \code{relative_AM_geotime()} is a wrapper on \code{relative_AM()}
#'   that calls function \code{sun_elevation()} to obtain the apparent position
#'   of the sun from the geographic and time coordinates. Function
#'   \code{relative_AMt_geotime()} is a wrapper on \code{relative_AMt()} that
#'   calls function \code{sun_elevation()} to obtain the true position of the
#'   sun from the geographic and time coordinates. At very low sun elevations
#'   the values returned by these two functions differ slightly because of the
#'   use of different approximations to correct for atmospheric refraction.
#'
#' @note Although relative air mass is not defined when the sun is not visible,
#'   returning a value different from the default \code{NA} might be useful in
#'   some cases and made possible by passing an argument to parameter
#'   \code{occluded.value}.
#'
#' @seealso \code{\link{sun_angles}}
#'
#' @return A numeric vector with the relative air mass values.
#'
#' @export
#'
#' @references
#' F. Kasten, A. T. Young (1989) Revised optical air mass tables and
#' approximation formula. Applied Optics, 28, 4735-4738. \doi{10.1364/AO.28.004735}.
#'
#' Young, A. T. (1994) Air mass and refraction. Applied Optics, 33, 1108-1110.
#' \doi{10.1364/AO.33.001108}
#'
#' @examples
#' # using the apparent sun elevation
#' relative_AM(elevation.angle = c(90, 60, 30, 1, -10))
#' relative_AM(elevation.angle = c(90, 60, 30, 1, -10),
#'             occluded.value = Inf)
#' relative_AM(zenith.angle = 0)
#'
#' # using the true or astronomical sun elevation
#' relative_AMt(elevation.angle = c(90, 60, 30, 1, -10))
#' relative_AMt(elevation.angle = c(90, 60, 30, 1, -10),
#'             occluded.value = Inf)
#' relative_AMt(zenith.angle = 0)
#'
#' # Define example geographic and time coordinates
#' baires.geo <-
#'   data.frame(lat = 34.60361, lon = -58.38139, address = "Buenos Aires")
#'
#' # using time and geographic coordinates
#' library(lubridate)
#' relative_AM_geotime(ymd_hms("2014-06-23 12:00:00",
#'                             tz = "America/Argentina/Buenos_Aires"),
#'                     geocode = baires.geo)
#' relative_AMt_geotime(ymd_hms("2014-06-23 12:00:00",
#'                              tz = "America/Argentina/Buenos_Aires"),
#'                     geocode = baires.geo)
#' relative_AM_geotime(ymd_hms("2014-06-23 12:00:00",
#'                             tz = "America/Argentina/Buenos_Aires") +
#'                       hours(0:12),
#'                     geocode = baires.geo)
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
           (sin(elevation.angle * pi / 180) +
              (0.1500 * (elevation.angle + 3.885)^-1.253))^-1,
           occluded.value[1]),
    3)
}

#' @rdname relative_AM
#'
#' @export
#'
relative_AMt <- function(elevation.angle = NULL,
                         zenith.angle = NULL,
                         occluded.value = NA_real_) {
  stopifnot(xor(is.null(elevation.angle), is.null(zenith.angle)))
  if (is.null(zenith.angle)) {
    zenith.angle <- 90 - elevation.angle
  }
  stopifnot(all(zenith.angle >= 0 & zenith.angle <= 180))
  cos_z <- cos(zenith.angle * pi / 180)
  signif(
    ifelse(zenith.angle <= 90.57, # refraction makes the sun visible
           (1.002432 * cos_z^2 + 0.148386 * cos_z + 0.0096467) /
             (cos_z^3 + 0.149864 * cos_z^2 + 0.0102963 * cos_z + 0.000303978),
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
           occluded.value = NA_real_) {
    relative_AM(sun_elevation(time = time,
                              tz = tz,
                              geocode = geocode,
                              use.refraction = TRUE),
                occluded.value = occluded.value)
  }

#' @rdname relative_AM
#'
#' @export
#'
relative_AMt_geotime <-
  function(time = lubridate::now(tzone = "UTC"),
           tz = lubridate::tz(time),
           geocode = tibble::tibble(lon = 0, lat = 51.5, address = "Greenwich"),
           occluded.value = NA_real_) {
    relative_AMt(sun_elevation(time = time,
                               tz = tz,
                               geocode = geocode,
                               use.refraction = FALSE),
                 occluded.value = occluded.value)
  }
