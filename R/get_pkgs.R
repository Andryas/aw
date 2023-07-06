#' @title Get Packages
#'
#' @description
#' This functions get all the packages used at a script R
#' based in the follow sintax `pkgs` :: `function()`.
#'
#' @param path path to script R
#'
#' @export
get_pkgs <- function(path) {
    x <- readLines(path)
    x <- trimws(stringr::str_extract_all(x, "[A-z.]+(?=::)", simplify = TRUE))
    x <- unique(as.vector(x))
    x <- x[-which(x == "")]
    return(x)
}
