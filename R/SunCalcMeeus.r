#' @details Please see the vignette \emph{0: The R for
#'   Photobiology Suite} for a description of the suite.
#'
#' @references
#' Aphalo, Pedro J. (2015) The r4photobiology suite. \emph{UV4Plants Bulletin}, 2015:1,
#' 21-29. \doi{10.19232/uv4pb.2015.1.14}.
#'
#' @import tibble
#'
#' @examples
#' # daylength
#' sunrise_time(lubridate::today(tzone = "EET"), tz = "EET",
#'              geocode = data.frame(lat = 60, lon = 25),
#'              unit.out = "hour")
#' day_length(lubridate::today(tzone = "EET"), tz = "EET",
#'            geocode = data.frame(lat = 60, lon = 25),
#'            unit.out = "hour")
#' sun_angles(lubridate::now(tzone = "EET"), tz = "EET",
#'            geocode = data.frame(lat = 60, lon = 25))
#'
"_PACKAGE"
