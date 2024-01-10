#' Standardize Text
#'
#' This function standardizes text by replacing spaces or specified patterns with a specified character
#' and converting the text to lowercase with Latin-ASCII transliteration.
#'
#' @param text A character vector containing the text to be standardized.
#' @param pattern A regular expression pattern specifying the characters to be replaced. Defaults to "\\s+" (spaces).
#' @param replace The character to replace the specified pattern in the text. Defaults to "_".
#'
#' @return A standardized version of the input text with replaced spaces or patterns and converted to lowercase.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Replaces consecutive dashes, single quotes, backticks, and double quotes with spaces.
#'   \item Converts the text to lowercase using Latin-ASCII transliteration.
#'   \item Replaces spaces or specified patterns with the specified replacement character.
#' }
#'
#' @examples
#' \dontrun{
#' # Standardize text with default parameters
#' standardized_text <- st("Hello World!")
#'
#' # Standardize text with custom pattern and replacement character
#' customized_text <- st("Custom Text with Patterns and Spaces", pattern = "\\W+", replace = "-")
#' }
#'
#' @export
st <- function(text, pattern = "\\s+", replace = "_") {
    text <- stringr::str_replace_all(text, "[-'`\"]+", " ")
    text <- stringr::str_replace_all(stringi::stri_trans_general(tolower(text), "Latin-ASCII"), pattern, replace)
    return(text)
}
