#' @title Format Numeric
#'
#' @description
#' Convert machine numbers (too big or too small) to human readable numbers.
#'
#' @param x the value
#' @param pre before value
#' @param pos after value
#' @param signif digits (default: 1)
#'
#' @examples
#'
#' library(ggplot2)
#' ggplot(cars, aes(x = dist, y = speed)) +
#'   geom_point() +
#'   scale_y_continuous(label = format_num)
#'
#' format_num(seq(100000, 1000000, 100000))
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
