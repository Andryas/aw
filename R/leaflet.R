#' Create a custom Leaflet icon with optional fill color.
#'
#' @param icon A character vector specifying the icon name.
#' @param fill A character vector specifying the fill color (default is NULL).
#' @param iconWidth An integer specifying the icon width (default is 48).
#' @param iconHeight An integer specifying the icon height (default is 48).
#'
#' @return A Leaflet icon object.
#'
#' @export
makeCustomIcon <- function(icon, fill = NULL, iconWidth = 48, iconHeight = 48) {
    icon <- icon[1]
    fill <- fill[1]
    iconWidth <- iconWidth[1]
    iconHeight <- iconHeight[1]

    # Bootstrap
    source <- "https://raw.githubusercontent.com/twbs/icons/main/icons/{icon}.svg"

    if (!dir.exists("~/.aw/svg/bootstrap")) {
        dir.create("~/.aw/svg/bootstrap", recursive = TRUE)
    }

    file_path <- paste0("~/.aw/svg/bootstrap/", icon, "_", iconHeight, "x", iconWidth, ".svg")
    if (!file.exists(file_path)) {
        download.file(
            url = stringr::str_glue(source),
            destfile = file_path
        )
    }

    tmp <- tempfile(fileext = ".svg")

    if (!is.null(fill)) {
        suppressWarnings({
            x <- readLines(file_path)
        })
        x <- purrr::map(x, stringr::str_replace, "\\bcurrentColor\\b", fill)
        x <- paste0(x, collapse = "\n")
        write(x, tmp)
    }

    leaflet::makeIcon(
        iconUrl = tmp,
        iconWidth = iconWidth,
        iconHeight = iconHeight
    )
}


#' Add custom markers to a Leaflet map.
#'
#' @param map A Leaflet map object.
#' @param data A data frame containing marker information.
#'
#' @return A Leaflet map object with custom markers added.
#'
#' @examples
#'
#' fill_tb <- tibble::tibble(
#'     type = unique(centris_ca$type)
#' ) |>
#'     dplyr::mutate(
#'         fill = RColorBrewer::brewer.pal(length(unique(type)), "Set1")
#'     )
#'
#' map <- leaflet::leaflet() |>
#'     leaflet::addTiles()
#'
#' data <- centris_ca |>
#'     dplyr::mutate(
#'         iconWidth = 28,
#'         iconHeight = 28,
#'         icon = "geo-fill"
#'     ) |>
#'     dplyr::left_join(
#'         fill_tb,
#'         by = "type"
#'     )
#'
#' map |>
#'     addCustomMarkers(data) |>
#'     leaflet::addLegend(
#'         colors = fill_tb$fill,
#'         labels = fill_tb$type
#'     )
#'
#' fill_tb$icon <- c("building", "building", "building", "house-fill", "building", "building", "pin-map", "geo-fill")
#'
#' data <- centris_ca |>
#'     dplyr::mutate(
#'         iconWidth = 28,
#'         iconHeight = 28
#'     ) |>
#'     dplyr::left_join(
#'         fill_tb,
#'         by = "type"
#'     )
#'
#' map |>
#'     addCustomMarkers(data) |>
#'     leaflet::addLegend(
#'         colors = fill_tb$fill,
#'         labels = fill_tb$type
#'     ) |>
#'     leaflet::addLayersControl(
#'         overlayGroups = fill_tb$icon,
#'         options = leaflet::layersControlOptions(collapsed = FALSE),
#'         position = "bottomleft"
#'     )
#' @export
addCustomMarkers <- function(map, data) {
    if (!("fill" %in% colnames(data))) {
        data$fill <- "#000000"
    }
    if (!("iconWidth" %in% colnames(data))) {
        data$iconWidth <- 48
    }
    if (!("iconHeight" %in% colnames(data))) {
        data$iconHeight <- 48
    }
    if (!("group" %in% colnames(data)) & !("icon" %in% colnames(data))) {
        data$group <- data$icon <- "geo-alt-fill"
    }
    if (!("icon" %in% colnames(data))) {
        data$icon <- "geo-alt-fill"
    }
    if (!("group" %in% colnames(data))) {
        data$group <- "default"
    }

    data |>
        dplyr::group_by(fill, icon) |>
        dplyr::group_split() |>
        purrr::walk(function(.x) {
            map <<- map |>
                leaflet::addMarkers(
                    data = .x,
                    lng = ~lon,
                    lat = ~lat,
                    group = ~group,
                    icon = ~ makeCustomIcon(.x$icon, .x$fill, .x$iconWidth, .x$iconHeight)
                )
        })

    return(map)
}
