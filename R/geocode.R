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
