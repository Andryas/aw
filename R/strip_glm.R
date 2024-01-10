#' Strip Unused Components from a Generalized Linear Model Object
#'
#' This function removes unnecessary components from a generalized linear model (GLM) object,
#' reducing its size and memory footprint. It is particularly useful when saving GLM objects
#' in Shiny apps or other scenarios where minimizing object size is important.
#'
#' @param cm A generalized linear model object to be stripped.
#'
#' @return A modified GLM object with unnecessary components removed to reduce its size.
#'
#' @details
#' The function removes the following components from the GLM object:
#' \itemize{
#'   \item \code{y} and \code{model} elements
#'   \item \code{residuals}, \code{fitted.values}, \code{effects}, \code{qr$qr}, \code{linear.predictors},
#'   \code{weights}, \code{prior.weights}, and \code{data} elements
#'   \item \code{family} components (\code{variance}, \code{dev.resids}, \code{aic}, \code{validmu}, \code{simulate})
#'   \item Attributes of \code{terms} and \code{formula}
#' }
#'
#' @examples
#' \dontrun{
#' # Strip unused components from a GLM object
#' stripped_glm <- strip_glm(fitted_glm)
#' }
#'
#' @export
strip_glm <- function(cm) {
    # https://stackoverflow.com/questions/36305062/how-can-i-reduce-the-size-of-a-linear-model-saved-by-a-shiny-app
    cm$y <- c()
    cm$model <- c()

    cm$residuals <- c()
    cm$fitted.values <- c()
    cm$effects <- c()
    cm$qr$qr <- c()
    cm$linear.predictors <- c()
    cm$weights <- c()
    cm$prior.weights <- c()
    cm$data <- c()


    cm$family$variance <- c()
    cm$family$dev.resids <- c()
    cm$family$aic <- c()
    cm$family$validmu <- c()
    cm$family$simulate <- c()
    attr(cm$terms, ".Environment") <- c()
    attr(cm$formula, ".Environment") <- c()

    cm
}
