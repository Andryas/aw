#' @title K Group
#'
#' @description
#' This functions create k groups equals
#'
#' @param n the total observations
#' @param k the total number of groups desired
#'
#' @examples
#'
#' k_groups(10, 10)
#' k_groups(100, 12)
#'
#' @export
k_groups <- function(n, k) {
    return(ceiling((1:n) / (n / k)))
}
