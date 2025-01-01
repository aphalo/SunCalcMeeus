
# SunCalcMeeus <img src="man/figures/logo.png" align="right" width="120"/>

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-last-release/SunCalcMeeus)](https://cran.r-project.org/package=SunCalcMeeus)
[![cran
checks](https://badges.cranchecks.info/worst/SunCalcMeeus.svg)](https://cran.r-project.org/web/checks/check_results_SunCalcMeeus.html)
[![SunCalcMeeus status
badge](https://aphalo.r-universe.dev/badges/SunCalcMeeus)](https://aphalo.r-universe.dev/SunCalcMeeus)
[![R-CMD-check](https://github.com/aphalo/SunCalcMeeus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aphalo/SunCalcMeeus/actions/workflows/R-CMD-check.yaml)
[![Documentation](https://img.shields.io/badge/documentation-SunCalcMeeus-informational.svg)](https://docs.r4photobiology.info/SunCalcMeeus/)
[![doi](https://img.shields.io/badge/doi-10.32614/CRAN.package.SunCalcMeeus-blue.svg)](https://doi.org/10.32614/CRAN.package.SunCalcMeeus)
<!-- badges: end -->

## Purpose

Package ‘SunCalcMeeus’ implements Meeus’ accurate algorithms for the
computation of the position of the sun, usable far into the past and
future. It also implements the computation of local solar time, day
length, night length and times of sunset and sunrise, based on accepted
and arbitrary sun elevation angles for twilight. Atmospheric refraction
makes the apparent sun elevation, as seen be an observer on Earth,
slightly different to the true astronomical sun elevation. The apparent
sun elevation differs from the true astronomical position because the
solar light beam changes its direction when travelling through the
atmosphere. At low sun elevations at high latitudes a rather small
difference expressed as an elevation angle becomes important for the
apparent sunrise and sunset times. The exact value is difficult to
compute with high accuracy as it depends on the water content in the
atmosphere. The correction applied here assumes a “normal” water vapour
column. Thus even if estimates of the true sun angles based on Meeus’
algorithm are reported to be accurate to 0.001 degrees, the estimates of
the apparent sun elevation can be in error by as much as 1 degree.

The ‘SunCalcMeeus’ package also implements Kasten and Young’s (1989)
approximation formula for optical air mass (AM) as a function of the
apparent sun elevation and Young’s (1994) follow-up approximate formula
as a function of the true sun elevation. In the first case the
correction for atmospheric refraction must be included in the
computation of the apparent elevation angle, in the second case this
correction is applied when computing AM.

Estimation of extraterrestrial irradiance (shortwave incoming radiation
at the top of the atmosphere) above a geographic location at the Earth
surface at a given instant in time is implemented in function
`irrad_extraterrestrial()`.

Class `tod_time` is defined and used to store time-of-day values based
on time zones. Classes `solar_time` and `solar_date` are defined and
used to store local solar times and dates, i.e., astronomical time
coordinates.

The same algorithms from Meeus’ book are used in the [NOAA Solar
Calculator](https://gml.noaa.gov/grad/solcalc/) of the United States
National Oceanic and Atmospheric Administration (NOAA), Global
Monitoring Laboratory. The version of the algorithm implemented in
‘SunCalcMeeus’ is not the simplified one used in the [worksheets
available from NOAA](https://gml.noaa.gov/grad/solcalc/calcdetails.html)
but instead nearly identical to that used in the on line calculator and
assumed to be accurate enough to be used confidently for times between
years 1000 BC to 3000 AC and with decreased accuracy even outside this
range.

The functions provided are vectorised, and the best performance per
computed value is achieved when long vectors of instants in time are
passed as arguments, as the equation of time is in this case generated
only once for each geographic location.

## History

This package is part of a suite of R packages for photobiological
calculations described at the
[r4photobiology](https://www.r4photobiology.info) web site and in the
vignette [The R for Photobiology
Suite](https://docs.r4photobiology.info/SunCalcMeeus/articles/userguide-0-r4p-introduction.html).

The functions now in ‘SunCalcMeeus’ were previously included in package
‘photobiology’. As they are being used also in other contexts, they are
now published separately in this R package to increase their visibility,
and facilitate their use and maintenance.

## Examples

Example of simple astronomical calculations for the sun.

``` r
library(SunCalcMeeus)
library(lubridate)
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union

# using times
geocode <- data.frame(lon = 0, lat = 55)
date <- lubridate::today(tzone = "UTC")

sunrise_time(date, geocode = geocode)
#> [1] "2024-12-31 08:24:57 UTC"

dates <- ymd("2024-09-21") + months(0:11)
sunrise_time(dates, geocode = geocode)
#>  [1] "2024-09-21 05:44:49 UTC" "2024-10-21 06:42:40 UTC"
#>  [3] "2024-11-21 07:44:40 UTC" "2024-12-21 08:23:23 UTC"
#>  [5] "2025-01-21 08:07:51 UTC" "2025-02-21 07:08:02 UTC"
#>  [7] "2025-03-21 05:58:42 UTC" "2025-04-21 04:41:25 UTC"
#>  [9] "2025-05-21 03:41:44 UTC" "2025-06-21 03:20:40 UTC"
#> [11] "2025-07-21 03:51:03 UTC" "2025-08-21 04:46:35 UTC"
```

``` r
day_length(dates, geocode = geocode)
#>  [1] 12.269699 10.061427  8.043433  7.164578  8.116912 10.184419 12.278813
#>  [8] 14.574059 16.496325 17.373085 16.513666 14.549905
```

``` r
times <- ymd_hms("2024-09-21 12:00:00") + minutes(seq(from = 0, to = 60, by = 10))
data.frame(time = times, elevation = sun_elevation(times, geocode = geocode))
#>                  time elevation
#> 1 2024-09-21 12:00:00  35.37991
#> 2 2024-09-21 12:10:00  35.28459
#> 3 2024-09-21 12:20:00  35.11304
#> 4 2024-09-21 12:30:00  34.86607
#> 5 2024-09-21 12:40:00  34.54482
#> 6 2024-09-21 12:50:00  34.15073
#> 7 2024-09-21 13:00:00  33.68555
```

``` r
sun_angles(times, geocode = geocode)
#> # A tibble: 7 × 12
#>   time                tz    solartime  longitude latitude address azimuth
#>   <dttm>              <chr> <solar_tm>     <dbl>    <dbl> <chr>     <dbl>
#> 1 2024-09-21 12:00:00 UTC   12:07:04           0       55 <NA>       182.
#> 2 2024-09-21 12:10:00 UTC   12:17:04           0       55 <NA>       185.
#> 3 2024-09-21 12:20:00 UTC   12:27:05           0       55 <NA>       188.
#> 4 2024-09-21 12:30:00 UTC   12:37:05           0       55 <NA>       191.
#> 5 2024-09-21 12:40:00 UTC   12:47:05           0       55 <NA>       194.
#> 6 2024-09-21 12:50:00 UTC   12:57:05           0       55 <NA>       197.
#> 7 2024-09-21 13:00:00 UTC   13:07:05           0       55 <NA>       200.
#> # ℹ 5 more variables: elevation <dbl>, declination <dbl>, eq.of.time <dbl>,
#> #   hour.angle <dbl>, distance <dbl>
```

## Installation

Installation of the most recent stable version from CRAN:

``` r
# not yet in CRAN!
# install.packages("SunCalcMeeus")
```

Installation of the current unstable version from R-Universe CRAN-like
repository:

``` r
install.packages('SunCalcMeeus', 
                 repos = c('https://aphalo.r-universe.dev', 
                           'https://cloud.r-project.org'))
```

Installation of the current unstable version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("aphalo/SunCalcMeeus")
```

## Documentation

HTML documentation is available at
(<https://docs.r4photobiology.info/SunCalcMeeus/>), including an *User
Guide*.

News on updates to the different packages of the ‘r4photobiology’ suite
are posted at (<https://www.r4photobiology.info/>).

Two articles introduce the basic ideas behind the design of the suite
and describe its use: Aphalo P. J. (2015)
(<https://doi.org/10.19232/uv4pb.2015.1.14>) and Aphalo P. J. (2016)
(<https://doi.org/10.19232/uv4pb.2016.1.15>).

A book is under preparation, and the draft is currently available at
(<https://leanpub.com/r4photobiology/>).

## Contributing

Pull requests, bug reports, and feature requests are welcome at
(<https://github.com/aphalo/SunCalcMeeus>).

## Citation

If you use this package to produce scientific or commercial
publications, please cite according to:

``` r
citation("SunCalcMeeus")
#> To cite package ‘SunCalcMeeus’ in publications use:
#> 
#>   Aphalo, Pedro J. (2015) The r4photobiology suite. UV4Plants Bulletin,
#>   2015:1, 21-29. DOI:10.19232/uv4pb.2015.1.14
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     author = {Pedro J. Aphalo},
#>     title = {The r4photobiology suite},
#>     journal = {UV4Plants Bulletin},
#>     volume = {2015},
#>     number = {1},
#>     pages = {21-29},
#>     year = {2015},
#>     doi = {10.19232/uv4pb.2015.1.14},
#>   }
```

## License

© 2012-2024 Pedro J. Aphalo (<pedro.aphalo@helsinki.fi>). Released under
the GPL, version 2 or greater. This software carries no warranty of any
kind.
