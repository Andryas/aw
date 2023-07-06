#' @title clean_list
#'
#' @description
#' Remove empty elements list and return the list only with the
#' elements that contains something inside.
#'
#' @param x a list
#'
#' @export
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
