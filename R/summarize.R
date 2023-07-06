#' @title simple_summarize
#'
#' @description
#' This function works only for numeric variables and
#' create a simple summary that contains min, q(0.25),
#' average, median, q(0.75), max, total_obs, total_na,
#' pct_na
#'
#' @param data dataset
#' @param vars passed to dplyr::summarize, it can be a vector buy
#' works only for numeric variables.
#' @param ... passed to dplyr::group_by, it can be a vector
#'
#' @export
simple_summarize <- function(data, vars, ...) {
    # https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
    p <- c(0.25, 0.75)
    p_names <- purrr::map_chr(p, ~ paste0("q", .x * 100))
    p_funs <- purrr::map(p, ~ purrr::partial(quantile, probs = .x)) |>
        purrr::set_names(nm = p_names)

    if (missing(...)) {
        purrr::map(
            eval(vars),
            function(.x) {
                data |>
                    dplyr::summarise_at(
                        dplyr::vars({{ .x }}),
                        # dplyr::across(
                        # {{ var }},
                        append(list(min = min, avg = mean, med = median, max = max, sd = sd), p_funs),
                        na.rm = TRUE
                        # )
                    ) |>
                    dplyr::select(min, q25, avg, med, q75, max, sd) |>
                    dplyr::bind_cols(
                        data |>
                            dplyr::summarise(
                                total_obs = dplyr::n(),
                                total_na = sum(is.na(!!sym(.x))),
                                pct_na = total_na / total_obs
                            )
                    ) |>
                    dplyr::mutate(var = .x) |>
                    dplyr::select(var, tidyselect::everything())
            }
        ) |>
            dplyr::bind_rows()
    } else {
        purrr::map(
            eval(vars),
            function(.x) {
                data |>
                    dplyr::group_by(...) |>
                    dplyr::summarise_at(
                        dplyr::vars({{ .x }}),
                        append(list(min = min, avg = mean, med = median, max = max, sd = sd), p_funs),
                        na.rm = TRUE
                    ) |>
                    dplyr::select(..., min, q25, avg, med, q75, max, sd) |>
                    dplyr::left_join(
                        data |>
                            dplyr::group_by(...) |>
                            dplyr::summarise(
                                total_obs = dplyr::n(),
                                total_na = sum(is.na(!!sym(.x))),
                                pct_na = total_na / total_obs
                            )
                    ) |>
                    dplyr::mutate(var = .x) |>
                    dplyr::select(var, tidyselect::everything())
            }
        ) |>
            dplyr::bind_rows()
    }
}

# simple_summarize(iris, var = eval(c("Sepal.Length", "Petal.Length")))
# simple_summarize(iris, var = eval(c("Sepal.Length", "Petal.Length")), Species)
