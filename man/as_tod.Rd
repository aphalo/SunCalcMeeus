% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time-of-day.r
\name{as_tod}
\alias{as_tod}
\title{Convert datetime to time-of-day}
\usage{
as_tod(x, unit.out = "hours", tz = NULL)
}
\arguments{
\item{x}{a datetime object accepted by lubridate functions.}

\item{unit.out}{character string, One of "tod_time", "hours", "minutes", or
"seconds".}

\item{tz}{character string indicating time zone to be used in output.}
}
\value{
A numeric vector of the same length as \code{x}. If
  \code{unit.out = "tod_time"} an object of class \code{"tod_time"} which
  a numeric vector as with \code{unit.out = "hours"} but with the class
  attribute set to \code{"tod_time"}, which dispatches to special
  \code{format()} and \code{print()} methods.
}
\description{
Convert a datetime into a time of day expressed in hours, minutes or seconds
from midnight in local time for a time zone. This conversion is useful when
time-series data for different days needs to be compared or plotted based on
the local time-of-day.
}
\examples{
library(lubridate)
my_instants <- ymd_hms("2020-05-17 12:05:03") + days(c(0, 30))
my_instants
as_tod(my_instants)
as_tod(my_instants, unit.out = "tod_time")

}
\seealso{
\code{\link{solar_time}}

Other Time of day functions: 
\code{\link{format.tod_time}()},
\code{\link{print.tod_time}()}
}
\concept{Time of day functions}
