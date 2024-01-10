# TODO add example
#' Generate Checkmark or Cross Icons in HTML for Boolean Values
#'
#' This function generates HTML code with checkmark or cross icons based on the input boolean values.
#'
#' @param x A logical vector representing boolean values to generate icons for.
#'
#' @return A character vector containing HTML code with checkmark or cross icons.
#'
#' @details
#' The function maps over the input boolean vector and generates HTML code with checkmark (green) or cross (red) icons.
#' It utilizes the `fontawesome` package to create the icons.
#'
#' @examples
#' \dontrun{
#' # Generate icons for boolean values
#' icons_html <- gt_check_uncheck(c(TRUE, FALSE, TRUE))
#' }
#'
#' @export
gt_check_uncheck <- function(x) {
    purrr::map(x, function(.x) {
        if (.x == TRUE) {
            logo_out <- fontawesome::fa("check", fill = "green")
        } else {
            logo_out <- fontawesome::fa("times", fill = "red")
        }

        logo_out %>%
            as.character() %>%
            gt::html()
    })
}
