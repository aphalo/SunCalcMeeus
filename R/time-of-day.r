#' Convert datetime to time-of-day
#'
#' Convert a datetime into a time of day expressed in hours, minutes or seconds
#' from midnight in local time for a time zone. This conversion is useful when
#' time-series data for different days needs to be compared or plotted based on
#' the local time-of-day.
#'
#' @param x a datetime object accepted by lubridate functions.
#' @param unit.out character string, One of "tod_time", "hours", "minutes", or
#'   "seconds".
#' @param tz character string indicating time zone to be used in output.
#'
#' @return A numeric vector of the same length as \code{x}. If
#'   \code{unit.out = "tod_time"} an object of class \code{"tod_time"} which
#'   a numeric vector as with \code{unit.out = "hours"} but with the class
#'   attribute set to \code{"tod_time"}, which dispatches to special
#'   \code{format()} and \code{print()} methods.
#'
#' @seealso \code{\link{solar_time}}
#'
#' @family Time of day functions
#'
#' @export
#'
#' @examples
#' library(lubridate)
#' my_instants <- ymd_hms("2020-05-17 12:05:03") + days(c(0, 30))
#' my_instants
#' as_tod(my_instants)
#' as_tod(my_instants, unit.out = "tod_time")
#'
as_tod <- function(x, unit.out = "hours", tz = NULL) {
  stopifnot(lubridate::is.timepoint(x))
  if (!is.null(tz)) {
    x <- lubridate::with_tz(x, tzone = tz[1])
  }
  if (unit.out == "tod_time") {
    tod <- lubridate::hour(x) + lubridate::minute(x) / 60 + lubridate::second(x) / 3600
    class(tod) <- c("tod_time", class(tod))
    tod
  } else if (unit.out == "hours") {
    lubridate::hour(x) + lubridate::minute(x) / 60 + lubridate::second(x) / 3600
  } else if (unit.out == "minutes") {
    lubridate::hour(x) * 60 + lubridate::minute(x) + lubridate::second(x) / 60
  } else if (unit.out == "seconds") {
    lubridate::hour(x) * 3600 + lubridate::minute(x) * 60 + lubridate::second(x)
  } else {
    stop("Unrecognized 'unit.out': ", unit.out)
  }
}

#' Encode in a Common Format
#'
#' Format a \code{tod_time} object into a character string for pretty printing.
#'
#' @param x an R object
#' @param ... ignored
#' @param sep character used as separator
#'
#' @return A character string with the time formatted as "HH:MM:SS", where ":"
#' is the argument passed to \code{sep}.
#'
#' @family Time of day functions
#'
#' @export
#'
format.tod_time <- function(x, ..., sep = ":") {
  hours <- as.integer(trunc(x))
  minutes <- as.integer((x * 60) %% 60)
  seconds <- as.integer((x * 3600) %% 60)
  fmt <- paste(rep("%02d", 3), collapse = sep)
  time_string <-
    sprintf(fmt = fmt, hours, minutes, seconds)
  time_string
}

#' Print time-of-day objects
#'
#' Defaults to print the underlying \code{numeric} vector as a solar time.
#'
#' @param x an R object
#' @param ... passed to \code{format} method
#'
#' @return Returns object \code{x}, invisibly.
#'
#' @family Time of day functions
#'
#' @export
#'
print.tod_time <- function(x, ...) {
  print(format(x, ...))
  invisible(x)
}
