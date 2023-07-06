#' @title Parse downloaded objects straight into R
#'
#' @description
#' Wrapper for \code{httr}'s \link[httr]{content}.  This is the default function used in \link{gcs_get_object}
#'
#' @param object The object downloaded
#' @param encoding Default to UTF-8
#'
#' @family download functions
#' @importFrom httr content
#' @export
gcs_parse_json <- function(x) {
    con <- gzcon(rawConnection(httr::content(x)))
    on.exit(close(con))
    jsonlite::fromJSON(con)
}

#' @title Object function to save RDS format
#'
#' @param input NULL
#' @param output NULL
#'
#' @export
gcs_save_object_rds <- function(input, output) {
    saveRDS(input, output)
}

#' @title Object function to save JSON format
#'
#' @param input NULL
#' @param output NULL
#'
#' @export
gcs_save_object_json <- function(input, output) {
    write(jsonlite::toJSON(input,
        pretty = TRUE,
        auto_unbox = TRUE
    ), output)
}

#' @title Geocode
#'
#' @param x address or a latitude and longitude separete by comma
#' @param key the gmap key
#'
#' @examples
#'
#' geocode('rua durval de morais 152 curitiba uberaba')
#'
#' @export
geocode <- function(x, key = NULL) {
    if (is.null(key)) key <- Sys.getenv("GMAPS")

    root <- "https://maps.googleapis.com/maps/api/geocode/json?key={key}&{parameter}={x}"

    x <- stringr::str_replace(x, "\\s+", "")
    if (stringr::str_detect(x, "^-?\\d{1,2}\\.\\d+,-?\\d{1,2}\\.\\d+$")) {
        # https://maps.googleapis.com/maps/api/geocode/json?latlng=40.714224,-73.961452&key=YOUR_API_KEY
        parameter <- "latlng"
        search <- stringr::str_glue(root)
    } else {
        parameter <- "address"
        x <- urltools::url_encode(x)
        search <- stringr::str_glue(root)
    }

    result <- jsonlite::read_json(search)
    result
}
