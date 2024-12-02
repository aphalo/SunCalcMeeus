#' Validate a geocode
#'
#' Test validity of a geocode or ensure that a geocode is valid.
#'
#' @details
#' \code{validate_geocode} Converts to tibble, checks data bounds, converts
#' address to character if it is not already a character vector, or add
#' character NAs if the address column is missing.
#'
#' \code{is_valid_geocode} Checks if a geocode is valid, returning 0L if not,
#' and the number of row otherwise.
#'
#' @param geocode data.frame with geocode data in columns \code{"lat"},
#'   \code{"lon"}, and possibly also \code{"address"}.
#'
#' @return A valid geocode stored in a tibble.
#'
#' @examples
#'
#' validate_geocode(NA)
#' validate_geocode(data.frame(lon = -25, lat = 66))
#'
#' is_valid_geocode(NA)
#' is_valid_geocode(1L)
#' is_valid_geocode(data.frame(lon = -25, lat = 66))
#'
#' na_geocode()
#'
#' @export
#'
validate_geocode <- function(geocode) {
  if (is.atomic(geocode) && (length(geocode) == 1L) && is.na(geocode)) {
    geocode <- na_geocode()
  } else if (is.data.frame(geocode)) {
    geocode <- tibble::as_tibble(geocode, .name_repair = "minimal")
  } else {
    stop("Bad geocode: ", format(geocode))
  }
  stopifnot(nrow(geocode) >= 1) # needs to be replace by generation of no output in all functions
  stopifnot(exists("lon", geocode), exists("lat", geocode))
  geocode[["lon"]] <- as.numeric(geocode[["lon"]]) # convert logical NA
  geocode[["lat"]] <- as.numeric(geocode[["lat"]]) # convert logical NA
  if (any(stats::na.omit(geocode[["lon"]]) > 180 | stats::na.omit(geocode[["lon"]]) < -180)) {
    stop("Longitude is off-range.")
  }
  if (any(stats::na.omit(geocode[["lat"]]) > 89.99 | stats::na.omit(geocode[["lat"]]) < -89.99)) {
    stop("Latitude is off-range.")
  }
  if (!exists("address", geocode)) {
    geocode[["address"]] <- NA_character_
  } else if (!is.character(geocode[["address"]])) {
    geocode[["address"]] <- as.character(geocode[["address"]])
  }
  geocode
}

#' @rdname validate_geocode
#'
#' @return FALSE for invalid, TRUE for valid.
#'
#' @export
#'
is_valid_geocode <- function(geocode) {
  if (!is.list(geocode)) return(FALSE)
  if (!is.data.frame(geocode)) {
    # walk list of geocodes using recursion
    is_valid <- all(sapply(geocode, is_valid_geocode))
  } else {
    is_valid <- nrow(geocode) >= 1L &&
      all(c("lon", "lat") %in% names(geocode)) &&
      all(c(is.numeric(geocode[["lon"]]), is.numeric(geocode[["lat"]]))) &&
      if ("address" %in% names(geocode)) is.character(geocode[["address"]]) else TRUE

    if (!is_valid && "address" %in% names(geocode) && is.factor(geocode[["address"]])) {
      warning("'address' is a factor instead of a character vector.")
    }
  }
  is_valid
}

#' @rdname validate_geocode
#'
#' @return FALSE for invalid, number of rows for valid.
#'
#' @export
#'
length_geocode <- function(geocode) {
  if (!is_valid_geocode(geocode)) return(NA_integer_)
  nrow(geocode)
}

#' @rdname validate_geocode
#'
#' @return A geo_code tibble with all fields set to suitable NAs.
#'
#' @export
#'
na_geocode <- function() {
  tibble::tibble(lon = NA_real_,
                 lat = NA_real_,
                 address = NA_character_)
}

