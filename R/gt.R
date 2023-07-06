#' @title GT check uncheck
#'
#' @param x TRUE for check, FALSE for times
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
