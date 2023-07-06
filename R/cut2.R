#' @title cut2
#'
#' @description
#' This function improve the readability of the cut breaks
#'
#' @param x @seealso cut
#' @param breaks @seealso cut
#' @param max_num convert the Inf to the maximum you want
#' @param ...
#'
#' @seealso cut
#'
#' @export
cut2 <- function(x, breaks, max_num = NULL, ...) {
  fx <- breaks
  fx_label <- format_num(fx)
  if (!is.null(max_num) & is.numeric(max_num)) {
    fx_label[fx_label == "Inf"] <- format_num(max_num)
  }
  fx_label <- stringr::str_c("[", fx_label[-length(fx_label)], "-", fx_label[-1], "]")
  cut(x, fx, fx_label, ...)
}
