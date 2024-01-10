#' Summarize Data with Descriptive Statistics and Quantiles
#'
#' This function provides a summary of the specified variables in the given data frame,
#' including descriptive statistics such as minimum, average, median, maximum, standard deviation,
#' and specified quantiles (defaulting to 25th and 75th percentiles).
#'
#' @param data A data frame containing the variables to be summarized.
#' @param vars The variables to be summarized. Can be specified as unquoted variable names.
#' @param ... Additional grouping variables if summarization is done by groups. Defaults to NULL.
#'
#' @return A data frame with summarized statistics for each variable, including minimum (min),
#' 25th percentile (q25), average (avg), median (med), 75th percentile (q75), maximum (max),
#' standard deviation (sd), total observations (total_obs), total missing values (total_na),
#' and percentage of missing values (pct_na).
#'
#' @examples
#' \dontrun{
#' # Summarize a single variable without grouping
#' summarize_stats(mtcars, mpg)
#'
#' # Summarize multiple variables by grouping
#' summarize_stats(mtcars, c(mpg, disp), cyl)
#' }
#'
#' @export
summarize_stats <- function(data, vars, ...) {
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
