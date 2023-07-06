#' @title Google Pronunciation Audio
#'
#' @description
#' Download the Google pronunciation audios.
#'
#' @param word a word or a vector of words
#' @param folder the path where it will be stored by the default is ~/Documents/google_pronunciation_audio
#' @param lang en (US) or fr (FR), default is en
#'
#' @examples
#'
#' google_pronunciation_audio(c("how", "whether"))
#' google_pronunciation_audio(c("maison", "velo"), lang = "fr")
#'
#' @export
google_pronunciation_audio <- function(word, folder = "~/Documents/google_audio_pronunciation/", lang = "en") {
    country <- ifelse(lang == "en", "us", "fr")
    source <- "https://ssl.gstatic.com/dictionary/static/pronunciation/2022-03-02/audio/${word2letters}/${word}_${lang}_${country}_1.mp3"
    if (stringr::str_detect(folder, "/$", negate = TRUE)) folder <- stringr::str_c(folder, "/")

    output <- purrr::map(word, function(.x) {
        source_word <- stringr::str_interp(source, list(word2letters = substr(.x, 1, 2), word = .x, lang = lang, country = country))
        file <- stringr::str_extract(source_word, "(?<=/)\\w+.mp3")
        status <- tryCatch({
            download.file(source_word, stringr::str_c(folder, file), "wget", quiet = TRUE)
            TRUE
        }, error = function(e) return(FALSE))
        Sys.sleep(1)
        tibble::tibble(word = .x, status = status)
    })

    dplyr::bind_rows(output)
}
