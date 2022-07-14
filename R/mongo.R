#' Get Data Mongo Iterable
#' 
#' This wrapper function of mongolite::mongo$iterate gets all 
#' the mongo data in a list controlling the page size.
#' 
#' @param con the mongolite::mongo connection
#' @param query see mongolite::mongo$find params
#' @param fields see mongolite::mongo$find params
#' @param stepsize the stepsize of each iteration, the maximum value 
#' per iteration is 1000 (Default: 1000)
#' @param progress print the progress (Default: TRUE)
#' 
#' @examples 
#' 
#' m <- mongolite::mongo()
#' mongo_find(m, '{"id": 1000}', '{"color": 1, "size": 1}')
#' 
#' @export
mongo_find <- function(con, query = "{}", fields = "{}", stepsize = 1000, progress = TRUE) {
    data <- list()

    N <- con$count()
    N_it <- ceiling(N / stepsize)
    i <- 1
    if (isTRUE(progress)) pb <- txtProgressBar(0, N_it, style = 3)
    while (i <= N_it) {
        if (isTRUE(progress)) setTxtProgressBar(pb, i)
        new_data <- con$iterate(
            query = "{}",
            fields = "{}",
            skip = length(data),
            limit = stepsize
        )
        new_data <- new_data$batch()
        data <- append(data, new_data)
        i <- i + 1
    }

    return(data)
}