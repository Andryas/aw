#' @title Strip GLM
#'
#' @description
#' Reduce the model size fitted by glm or lm, getting the
#' coefficients and using only a predictive model.
#'
#' @param cm a lm or glm object
#'
#' @export
strip_glm <- function(cm) {
    # https://stackoverflow.com/questions/36305062/how-can-i-reduce-the-size-of-a-linear-model-saved-by-a-shiny-app
    cm$y = c()
    cm$model = c()

    cm$residuals = c()
    cm$fitted.values = c()
    cm$effects = c()
    cm$qr$qr = c()
    cm$linear.predictors = c()
    cm$weights = c()
    cm$prior.weights = c()
    cm$data = c()


    cm$family$variance = c()
    cm$family$dev.resids = c()
    cm$family$aic = c()
    cm$family$validmu = c()
    cm$family$simulate = c()
    attr(cm$terms, ".Environment") = c()
    attr(cm$formula, ".Environment") = c()

    cm
}
