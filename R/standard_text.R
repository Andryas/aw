#' @title standard_text
#'
#' @description
#' Standardize text with no accent and to lower.
#' Replace the spaces is optional
#'
#' @param text the string
#' @param pattern see str_replace_all
#' @param replace see str_replace_all
#'
#' @export
standard_text <- function(text, pattern = "\\s+", replace = "_") {
    text <- stringr::str_replace_all(text, "[-'`\"]+", " ")
    text <- stringr::str_replace_all(stringi::stri_trans_general(tolower(text), "Latin-ASCII"), pattern, replace)
    return(text)
}
