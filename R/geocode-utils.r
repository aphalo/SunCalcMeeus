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
  # if (is.list(geocode) && !is.data.frame(geocode)) {
  #   return(lapply(geocode, validate_geocode))
  # }
  if (is.atomic(geocode) && (length(geocode) == 1L) && is.na(geocode)) {
    geocode <- na_geocode()
  } else if (is.data.frame(geocode)) {
    geocode <- tibble::as_tibble(geocode, .name_repair = "minimal")
  } else {
    stop("Bad 'geocode': ", paste(format(geocode), collapse = ", ", sep = ""))
  }
  if (length(geocode) < 1L || nrow(geocode) < 1L) {
    if (is.data.frame(geocode)) {
      stop("'geocode' data frame has no rows")
    } else {
      stop("'geocode' is missing")
    }
  }
  if (!all(c("lon", "lat") %in% names(geocode))) {
    stop("Bad geocode names: ",
         paste(names(geocode), collapse = ", ", sep = ""))
  }
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

    # range test
    is_valid <- is_valid &&
      !any(stats::na.omit(geocode[["lon"]]) > 180 | stats::na.omit(geocode[["lon"]]) < -180)
    is_valid <- is_valid &&
      !any(stats::na.omit(geocode[["lat"]]) > 89.99 | stats::na.omit(geocode[["lat"]]) < -89.99)

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

#' Split or bind geocodes
#'
#' Multiple geocodes can be stored as rows in a single data frame or as a
#' named list of one row data frames. As metadata attributes lists are easier
#' to manipulate, but when computing the sun position data frames are more
#' efficient. These functions implement conversions between objects using
#' these two approaches.
#'
#' @param geocode data.frame with geocode data in columns \code{"lat"},
#'   \code{"lon"}, and possibly also \code{"address"}.
#' @param idFactor character Name of the column where the ID factor is stored
#'   in geocode data frames with multiple rows.
#' @param simplify logical Flag indicating if when the list to be returnes
#'   has a single member, the member geocode should be returned instead.
#'
#' @details Function \code{split_geocodes()} splits a multi-row data.frame
#'   containing one geocode per row and a \code{character} vector or a
#'   \code{factor} as ID column into a named list of one row data
#'   frames containing the same geocode information. If the input is a
#'   single-row data frame or a list with a single member, and
#'   \code{simplify = TRUE} is passed in the call, a bare data frame is
#'   returned, with the ID column, if present, deleted.
#'
#'   Function \code{bind_geocodes()} binds the geocodes members of a
#'   list into a multirow data.frame containing one geocode per row and
#'   a \code{character} vector ID column containing the same geocode
#'   information.
#'
#' @return A list of geo_code data frames or a single geocode data frame.
#'
#' @examples
#' my.geocodes <- data.frame(lon = c(0, 10, 15),
#'                           lat = c(30, 60, 89),
#'                           address = c("one", "two", "three"),
#'                           spct.idx = c("A", "B", "C"))
#'
#' split_geocodes(my.geocodes)
#' split_geocodes(my.geocodes[1, ])
#'
#' my.list <- list("A" = data.frame(lon = 10,
#'                                  lat = 30,
#'                                  address = "North"),
#'                 "B" = data.frame(lon = -10,
#'                                  lat = -30,
#'                                  address = "South"))
#'
#' bind_geocodes(my.list)
#'
#' @export
#'
split_geocodes <- function(geocode,
                           idFactor = "spct.idx",
                           simplify = TRUE) {
  if (is.data.frame(geocode)) {
    stopifnot(is_valid_geocode(geocode))
    if (idFactor %in% colnames(geocode)) {
      IDs <- as.character(geocode[[idFactor]])
    } else {
      stop("idFactor '", idFactor, "' not a column in 'geocode'")
    }
    temp.df <- geocode[ , -which(colnames(geocode) == idFactor)]
    geocode <- list()
    for (i in seq_along(IDs)) {
      geocode[[IDs[i]]] <- validate_geocode(temp.df[i, ])
    }
  }
  if (simplify &&
      is.list(geocode) &&
      (!is.data.frame(geocode)) &&
      length(geocode) == 1L) {
    geocode <- geocode[[1]]
  }
  geocode
}

#' @rdname split_geocodes
#'
#' @export
#'
bind_geocodes <- function(geocode,
                          idFactor = "spct.idx") {
  if (is.list(geocode) && !is.data.frame(geocode)) {
    if (length(unique(names(geocode))) != length(geocode)) {
      warning("Names of list members are missing or are not unique!")
    }
    geocode <- dplyr::bind_rows(geocode, .id = idFactor)
#    class(z) <- class(geocode[[1L]])
  }
  validate_geocode(geocode)
}
