#' Divide Sequence into K Groups
#'
#' This function divides a sequence of integers from 1 to n into k groups,
#' ensuring a balanced distribution across the groups as much as possible.
#'
#' @param n An integer specifying the length of the sequence.
#' @param k An integer specifying the number of groups to divide the sequence into.
#'
#' @return A numeric vector indicating the group assignment for each element in the sequence.
#'
#' @details
#' The function assigns each element in the sequence an integer value indicating its group assignment.
#' The assignment is done in such a way that each group contains approximately the same number of elements.
#' If n is not divisible by k, the remaining elements are distributed evenly across the groups.
#'
#' @examples
#' \dontrun{
#' # Divide a sequence of 10 integers into 3 groups
#' group_assignment <- k_groups(10, 3)
#' }
#'
#' @export
k_groups <- function(n, k) {
    return(ceiling((1:n) / (n / k)))
}

#' Extract Unique Package Names from a File
#'
#' This function reads a file and extracts unique package names defined within the file.
#' The function specifically looks for expressions of the form 'package::function' and extracts the package names.
#'
#' @param path A character string specifying the path to the file.
#'
#' @return A character vector containing unique package names extracted from the file.
#'
#' @details
#' The function reads the specified file, searches for expressions of the form 'package::function',
#' and extracts the package names. It returns a vector of unique package names, excluding any empty strings.
#'
#' @examples
#' \dontrun{
#' # Extract unique package names from a file
#' packages_used <- get_pkgs("path/to/R/script.R")
#' }
#'
#' @export
get_pkgs <- function(path) {
    x <- readLines(path)
    x <- trimws(stringr::str_extract_all(x, "[A-z.]+(?=::)", simplify = TRUE))
    x <- unique(as.vector(x))
    x <- x[-which(x == "")]
    return(x)
}

#' Format Numbers with Scale Abbreviations
#'
#' This function formats numeric values with appropriate scale abbreviations (e.g., k, m, bn, tn).
#'
#' @param x A numeric vector to be formatted.
#' @param pre A character string to be prefixed to the formatted number. Defaults to an empty string.
#' @param pos A character string to be suffixed to the formatted number. Defaults to an empty string.
#' @param signif An integer specifying the number of significant digits to round to. Defaults to 1.
#'
#' @return A character vector of formatted numbers with appropriate scale abbreviations.
#'
#' @details
#' The function formats numeric values with the following scale abbreviations:
#' - k: thousands
#' - m: millions
#' - bn: billions
#' - tn: trillions
#'
#' If the value is infinite, the result is unchanged. If the value is NA or NULL, the result is a hyphen ("-").
#'
#' @examples
#' \dontrun{
#' # Format numeric values with default parameters
#' formatted_numbers <- format_num(c(1000, 1500000, 3000000000))
#'
#' # Format numeric values with custom prefixes and suffixes
#' customized_format <- format_num(c(5000, 750000, 1200000000), pre = "$", pos = " USD")
#' }
#'
#' @export
format_num <- function(x = NULL, pre = "", pos = "", signif = 1) {
    sapply(x, function(x) {
        if (is.infinite(x)) {
            return(x)
        } else if (!is.na(x)) {
            tn <- round(abs(x) / 1e12, signif)
            b <- round(abs(x) / 1e9, signif)
            m <- round(abs(x) / 1e6, signif)
            k <- round(abs(x) / 1e3, signif)

            if (x >= 0) {
                x_is_positive <- ""
            } else {
                x_is_positive <- "-"
            }

            if (k < 1) {
                r <- paste0(x_is_positive, pre, round(abs(x), signif))
            } else if (m < 1) {
                r <- paste0(x_is_positive, pre, k, "k")
            } else if (b < 1) {
                r <- paste0(x_is_positive, pre, m, "m")
            } else if (tn < 1) {
                r <- paste0(x_is_positive, pre, b, "bn")
            } else {
                r <- paste0(x_is_positive, pre, scales::comma(tn), "tn")
            }

            return(paste0(r, pos))
        } else if (is.na(x) | is.null(x)) {
            return("-")
        }
    })
}

# TODO rewrite this function
# #' @title cut2
# #'
# #' @description
# #' This function improve the readability of the cut breaks
# #'
# #' @param x @seealso cut
# #' @param breaks @seealso cut
# #' @param max_num convert the Inf to the maximum you want
# #' @param ...
# #'
# #' @seealso cut
# #'
# #' @export
# cut2 <- function(x, breaks, max_num = NULL, ...) {
#     fx <- breaks
#     fx_label <- format_num(fx)
#     if (!is.null(max_num) & is.numeric(max_num)) {
#         fx_label[fx_label == "Inf"] <- format_num(max_num)
#     }
#     fx_label <- stringr::str_c("[", fx_label[-length(fx_label)], "-", fx_label[-1], "]")
#     cut(x, fx, fx_label, ...)
# }


#' Convert Degrees to Radians
#'
#' This function converts degrees to radians using the formula: \code{deg * (pi / 180)}.
#'
#' @param deg Numeric vector representing angles in degrees.
#'
#' @return Numeric vector representing angles converted to radians.
#'
#' @details
#' The function performs a simple conversion of angles from degrees to radians using the formula \code{deg * (pi / 180)}.
#'
#' @examples
#' \dontrun{
#' # Convert angles from degrees to radians
#' radians <- deg2rad(c(0, 45, 90, 180, 360))
#' }
#'
#' @export
deg2rad <- function(deg) {
    deg * (pi / 180)
}

#' Calculate Geographic Distance between Two Points
#'
#' This function calculates the geographic distance between two points specified by their latitude and longitude.
#'
#' @param lat1 Latitude of the first point.
#' @param lng1 Longitude of the first point.
#' @param lat2 Latitude of the second point.
#' @param lng2 Longitude of the second point.
#'
#' @return The geographic distance between the two points in kilometers.
#'
#' @details
#' The function uses the Haversine formula to calculate the distance between two points on the surface of a sphere,
#' such as the Earth. The result is the distance in kilometers.
#'
#' @examples
#' \dontrun{
#' # Calculate distance between two geographic points
#' distance_km <- geo_distance(37.7749, -122.4194, 34.0522, -118.2437)
#' }
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


clean_list <- function(x) {
    index <-
        which(purrr::simplify(purrr::map(
            x, ~ length(.x) == 0 | is.null(.x) || is.na(.x)
        )))
    if (length(index) > 0) {
        return(x[-index])
    } else {
        return(x)
    }
}
