#' @title deg2rad
#'
#' @description
#' degrees to radian
#'
#' @param deg degrees
#'
#' @export
deg2rad <- function(deg) {
    deg * (pi / 180)
}

#' @title Geographic Distance
#'
#' @description
#' Calculate the euclidian distance between to lat/lng point.
#'
#' @param lat1 latitude first position
#' @param lng1 longitude first position
#' @param lat2 latitude second position
#' @param lng2 longitude second position
#'
#' @return the result is in km
#'
#' @export
geo_distance <- function(lat1, lng1, lat2, lng2) {
    R <- 6371 # Radius of the earth in km
    dLat <- deg2rad(lat2 - lat1) # deg2rad below
    dLon <- deg2rad(lng2 - lng1)
    a <- sin(dLat / 2) * sin(dLat / 2) + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(dLon / 2) * sin(dLon / 2)
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    d <- R * c # Distance in km
    return(d)
}
