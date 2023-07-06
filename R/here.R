#' @title Geocode reverse
#'
#' @description
#' This function brings the information from a geolocation
#' reference point from a lat and lng.
#'
#' @param lat latitude
#' @param lng longitude
#' @param key the here key
#'
#' @export
here_geocode_reverse <- function(lat, lng, key = NULL) {
    if (is.null(key)) {
        key <- Sys.getenv("HEREKEY")
        if (nchar(key) == 0) stop("No key provider")
    }
    url <- stringr::str_c(
        "https://revgeocode.search.hereapi.com/v1/revgeocode?at=${lat},${lng}",
        "&apiKey=${key}"
    )
    # print(paste0("query address text: ", url))
    # text <- urltools::url_encode(text)
    json <- jsonlite::fromJSON(stringr::str_interp(url), simplifyVector = FALSE)
    jsons <- json[["items"]][[1]]
    if (length(jsons) > 0) {
        return(jsons)
    } else {
        return(NULL)
    }
}

#' @title Geocode
#'
#' @description
#' This function brings the information from a geolocation
#' reference point from a given address.
#'
#' @param address the address
#' @param key the here key
#'
#' @export
here_geocode <- function(address, key = NULL) {
    if (is.null(key)) {
        key <- Sys.getenv("HEREKEY")
        if (nchar(key) == 0) stop("No key provider")
    }
    url <- stringr::str_c(
        "https://geocode.search.hereapi.com/v1/geocode?q=${address}",
        "&apiKey=${key}"
    )
    url <- stringr::str_interp(url, list(address = urltools::url_encode(address), key = key))
    # url <- urltools::url_encode(url)
    json <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    jsons <- json[["items"]][[1]]
    if (length(jsons) > 0) {
        jsons <- json[["items"]][[1]]
        return(jsons)
    } else {
        return(NULL)
    }
}
