#' @title tryObserve
#'
#' @description
#' This function is a shiny::observe which in case of failure it
#' not freeze the application, but save the error in the database for later
#' analysis.
#'
#' @param x an expression
#' @param metadata a list containing the error's metadata
#' @param con the mongo connection to insert the error
#' @param w the connection with the waiter msg to display
#' @param w_msg the error msg. Only works if w is passed. By the default the message is: one error occurred
#'
#' @details
#' It is evaluate a variable in the .Renviron (PROD). When it is TRUE, the
#' error is saved at the mongo database, when is not it is only printed.
#'
#' @export
tryObserve <- function(x, metadata, con, w = NULL, w_msg = "One error occurred") {
    x <- substitute(x)
    env <- parent.frame()
    observe({
        tryCatch(
            eval(x, env),
            error = function(e) {
                if (!is.null(w)) w$update(html = w_msg)
                Sys.sleep(4)

                tmp <- tempfile(fileext = ".log")
                lf <- logr::log_open(tmp)
                logr::log_print(e)
                logr::log_close()

                if (isTRUE(as.logical(Sys.getenv("PROD")))) {
                    error <- readLines(lf)
                    con$insert(jsonlite::toJSON(append(metadata, list(error = error)), auto_unbox = TRUE))
                } else {
                    print(lf)
                }

                if (!is.null(w)) w$hide()

                return(NULL)
            }
        )
    })
}
