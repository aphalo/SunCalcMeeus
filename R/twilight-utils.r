#' twilight argument check and conversion
#'
#' @return numeric Solar elevation angle at sunrise or sunset
#' @keywords internal
#'
twilight2angle <- function(twilight) {
  # refraction depends on the elevation angle (ha), atmospheric pressure and temperature
  # we assume atmospheric pressure 1010 hPa and temperature 10 C
  # constants below are approximate
  # function atm_refraction_approx() can be used to estimate refraction for other angles
  if (length(twilight) == 0) {
    warning("'twilight' too short or NULL")
    twilight_angle <- c(NA_real_, NA_real_)
  } else if (is.character(twilight)) {
    if (length(twilight) != 1) {
      warning("'twilight' is character but of length > 1, using first value")
      twilight <- twilight[1L]
    }
    twilight_angle <-
      switch(twilight,
             none = c(0, 0), # center of solar disk
             rim = c(-0.53/2, -0.53/2), # upper rim of solar disk
             refraction = c(-0.4819444, -0.4819444), # center of solar disk, refraction corrected
             sunlight = c(-0.833, -0.833),
             civil = c(-6, -6),
             nautical = c(-12, -12),
             astronomical = c(-18, -18),
             {warning("Unrecognised 'twilight' value: ", twilight); c(NA_real_, NA_real_)}
      )
  } else if (is.numeric(twilight)) {
    if (length(twilight) == 1) {
      twilight_angle <- rep(twilight, 2)
    } else if (length(twilight) == 2) {
      twilight_angle <- twilight
    } else {
      stop("'twilight' vector longer than 2")
    }
    if (any(!is.na(twilight_angle) &
            twilight_angle < -90 | twilight_angle > 90 ) ) {
      warning("Off range twilight angle(s) replaced with NAs.")
      twilight_angle <-
        ifelse(!is.na(twilight_angle) &
                 twilight_angle > 90 | twilight_angle < -90,
               NA_real_, twilight_angle)
    }
  } else {
    warning("'twilight' must be numeric or character, but is ", class(twilight))
    twilight_angle <- c(NA_real_, NA_real_)
  }
  twilight_angle
}
