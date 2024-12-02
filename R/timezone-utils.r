#' Time difference between two time zones
#'
#' Returns the difference in local time expressed in hours between two time
#' zones at a given instant in time. The difference due to daylight saving time
#' or Summer and Winter time as well as historical changes in time zones are
#' taken into account.
#'
#' @note This function is implemented using functions from package 'lubridate'.
#'   For details on the handling of time zones, please, consult the
#'   documentation for \code{\link{Sys.timezone}} about system differences in
#'   time zone names and handling.
#'
#' @param when datetime A time instant
#' @param tz.target,tz.reference character Two time zones using names
#' recognized by functions from package 'lubridate'
#'
#' @return A \code{numeric} value.
#'
#' @export
#'
tz_time_diff <- function(when = lubridate::now(),
                         tz.target = lubridate::tz(when),
                         tz.reference = "UTC") {
  if (lubridate::is.Date(when)) {
    when <- lubridate::as_datetime(when, tz = tz.target)
  }
  (as.numeric(lubridate::force_tz(when, tz.reference)) -
      as.numeric(lubridate::force_tz(when, tz.target))) / 3600
}
