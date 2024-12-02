#' Local solar time
#'
#' \code{solar_time()} computes the time of day expressed in seconds since the
#' astronomical midnight using and instant in time and a geocode as input. Solar
#' time is useful when we want to plot data according to the local solar time
#' rather than the local time in use at a time zone. How the returned instant in
#' time is expressed depends on the argument passed to \code{unit.out}.
#'
#' @details Solar time is determined by the position of the sun in the sky and
#' it almost always differs from the time expressed in the local time
#' coordinates in use. The differences can vary from a few minutes up to a
#' couple of hours depending on the exact location within the time zone and the
#' use or not of daylight saving time.
#'
#' @param time POSIXct Time, any valid time zone (TZ) is allowed, default is
#'   current time
#' @param geocode data frame with variables lon and lat as numeric values
#'   (degrees).
#' @param unit.out character string, One of "datetime", "time", "hour", "minute", or
#'   "second".
#'
#' @seealso \code{\link{as_tod}}
#'
#' @family Local solar time functions
#'
#' @note The algorithm is approximate, it calculates the difference between
#'   local solar noon and noon in the time zone of \code{time} and uses this
#'   value for the whole day when converting times into solar time. Days are not
#'   exactly 24 h long. Between successive days the shift is only a few seconds,
#'   and this leads to a small jump at midnight.
#'
#' @section Warning!: Returned values are computed based on the time zone of the
#'   argument for parameter time. In the case of solar time, this timezone does
#'   not affect the result. However, in the case of solar dates the date part
#'   may be off by one day, if the time zone does not match the coordinates of
#'   the geocode value provided as argument.
#'
#' @return In all cases solar time is expressed as time since local astronomical
#'   midnight and, thus, lacks date information. If \code{unit.out = "time"}, a
#'   numeric value in seconds with an additional class attribute
#'   "solar_time"; if \code{unit.out = "datetime"}, a "POSIXct" value in seconds
#'   from midnight but with an additional class attribute "solar_date"; if
#'   \code{unit.out = "hour"} or \code{unit.out = "minute"} or \code{unit.out =
#'   "second"}, a numeric value.
#'
#' @export
#'
#' @examples
#' BA.geocode <-
#'   data.frame(lon = -58.38156, lat = -34.60368, address = "Buenos Aires, Argentina")
#' sol_t <- solar_time(lubridate::dmy_hms("21/06/2016 10:00:00", tz = "UTC"),
#'                     BA.geocode)
#' sol_t
#' class(sol_t)
#'
#' sol_d <- solar_time(lubridate::dmy_hms("21/06/2016 10:00:00", tz = "UTC"),
#'                     BA.geocode,
#'                     unit.out = "datetime")
#' sol_d
#' class(sol_d)
#'
solar_time <- function(time = lubridate::now(),
                       geocode = tibble::tibble(lon = 0,
                                                lat = 51.5,
                                                address = "Greenwich"),
                       unit.out = "time")
{
  # solar time in hours from midnight
  solar.time <- sun_angles(time = time,
                           tz = lubridate::tz(time),
                           geocode = geocode)[["solartime"]]
  switch(unit.out,
         "date" = as.solar_date(solar.time, time),
         "datetime" = as.solar_date(solar.time, time),
         "time" = solar.time,
         "days" = as.numeric(solar.time) / 24,
         "hours" = as.numeric(solar.time),
         "minutes" = as.numeric(solar.time * 60),
         "seconds" = as.numeric(solar.time * 3600)
  )
}

#' Convert a solar_time object into solar_date object
#'
#' @param x solar_time object.
#' @param time an R date time object
#'
#' @return For method \code{as.solar_date()} a date-time object with the class attr
#'   set to "solar.time". This is needed only for unambiguous formatting and
#'   printing.
#'
#' @family Local solar time functions
#'
#' @export
#'
as.solar_date <- function(x, time)
{
  stopifnot(is.solar_time(x))
  stopifnot(lubridate::is.timepoint(time))
  solar.date <-
    lubridate::floor_date(time, unit = "days") +
    lubridate::seconds(as.numeric(x) * 3600)
  class(solar.date) <- c("solar_date", class(solar.date))
  solar.date
}

#' Query class
#'
#' @param x an R object.
#'
#' @family Local solar time functions
#'
#' @export
#'
is.solar_time <- function(x) {
  inherits(x, "solar_time")
}

#' @rdname is.solar_time
#'
#' @export
#'
is.solar_date <- function(x) {
  inherits(x, "solar_date")
}

#' Encode in a Common Format
#'
#' Format a \code{solar_time} object for pretty printing
#'
#' @param x an R object
#' @param ... ignored
#' @param sep character used as separator
#'
#' @family astronomy related functions
#'
#' @export
#'
format.solar_time <- function(x, ..., sep = ":") {
  hours <- as.integer(trunc(x))
  minutes <- as.integer((x * 60) %% 60)
  seconds <- as.integer((x * 3600) %% 60)
  fmt <- paste(rep("%02d", 3), collapse = sep)
  time_string <-
    sprintf(fmt = fmt, hours, minutes, seconds)
  time_string
}

#' Print solar time and solar date objects
#'
#' @param x an R object
#' @param ... passed to \code{format} method
#'
#' @family Local solar time functions
#'
#' @note Default is to print the underlying POSIXct as a solar time.
#'
#' @export
#'
print.solar_time <- function(x, ...) {
  print(format(x, ...))
  invisible(x)
}

#' @rdname print.solar_time
#'
#' @export
#'
print.solar_date <- function(x, ...) {
  print(paste(format(x, ...), "solar"))
  invisible(x)
}

