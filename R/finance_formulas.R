#' @title Monthly Interest
#'
#' @param x a annual interest to interest monthly, where 0.10 equals 10%.
#'
#' @export
monthly_interest <- function(x) {
    return((1 + x)^(1 / 12) - 1)
}

#' @title Annual Interest
#'
#' @param x a monthly interest to annualize, where 0.10 equals 10%.
#'
#' @export
annualize_interest <- function(x) {
    return(prod(1 + x)^(12) - 1)
}

#' @title XIRR
#'
#' @description
#' Calculates a XIRR for a given cash flow
#'
#' @param x the cumulative cash flow
#'
#' @export
xirr <- function(x) {
    r <- jrvFinance::irr(x)
    return((1 + r)^12 - 1)
}
