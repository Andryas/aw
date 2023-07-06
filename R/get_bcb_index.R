#' @title get_bcb_index
#'
#' @description
#' This functions get the Brazil index through the bcb api.
#'
#' @param id_code search id_code in https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries
#' @param start the day to start (default 2000-01-01)
#' @param end the last day (default as.character(Sys.Date()))
#'
#' @export
get_bcb_index <- function(id_code, start = '01/01/2000', end = as.character(format(Sys.Date(), "%d/%m/%Y"))) {
    url <- "http://api.bcb.gov.br/dados/serie/bcdata.sgs.${id_code}/dados?formato=json&dataInicial=${start}&dataFinal=${end}"
    data <- jsonlite::fromJSON(stringr::str_interp(url, list(id_code = id_code, start = start, end = end)))
    data <- dplyr::as_tibble(data) |>
        dplyr::mutate(date = lubridate::dmy(data), y = as.numeric(valor) / 100) |>
        dplyr::select(-data, -valor)
    return(data)
}
