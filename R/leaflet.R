#' Create a custom leaflet icon.
#'
#' This function downloads SVG icons from Font Awesome or Bootstrap Icons based on the specified source and icon name.
#'
#' @param icon Character vector containing the name of the icon.
#' @param fill Character vector specifying the color to fill the icon. Default is NULL.
#' @param iconWidth Numeric vector specifying the width of the icon. Default is 48.
#' @param iconHeight Numeric vector specifying the height of the icon. Default is 48.
#' @param source Character vector specifying the source of the icon ("fontawesome" or "bootstrap"). Default is "fontawesome".
#' @param solid If source "fontawesome", a logical vector indicating whether to use solid icons (TRUE) or regular icons (FALSE). Default is TRUE.
#'
#' @return A leaflet icon object.
#'
#' @export
makeCustomIcon <- function(icon, fill = NULL, iconWidth = 48, iconHeight = 48, source = "fontawesome", solid = TRUE) {
    icon <- icon[1]
    fill <- fill[1]
    iconWidth <- iconWidth[1]
    iconHeight <- iconHeight[1]
    solid <- solid[1]
    source <- source[1]
    solid <- ifelse(isTRUE(solid), "solid", "regular")

    if (!dir.exists(stringr::str_glue("~/.aw/svg/{source}"))) {
        dir.create(stringr::str_glue("~/.aw/svg/{source}"), recursive = TRUE)
    }

    file_path <- stringr::str_glue(paste0("~/.aw/svg/{source}/", icon, "_", iconHeight, "x", iconWidth, ".svg"))
    if (!file.exists(file_path)) {
        if (source == "fontawesome") {
            status <- tryCatch(
                {
                    suppressWarnings(
                        download.file(
                            url = stringr::str_glue("https://raw.githubusercontent.com/FortAwesome/Font-Awesome/master/svgs/brands/{icon}.svg"),
                            destfile = file_path,
                            quiet = TRUE
                        )
                    )

                    TRUE
                },
                error = function(e) FALSE
            )

            if (isFALSE(status)) {
                download.file(
                    url = stringr::str_glue("https://raw.githubusercontent.com/FortAwesome/Font-Awesome/master/svgs/{solid}/{icon}.svg"),
                    destfile = file_path,
                    quiet = TRUE
                )
            }
        } else {
            download.file(
                url = stringr::str_glue("https://raw.githubusercontent.com/twbs/icons/main/icons/{icon}.svg"),
                destfile = file_path
            )
        }
    }

    tmp <- tempfile(fileext = ".svg")

    if (!is.null(fill)) {
        suppressWarnings({
            x <- readLines(file_path)
        })
        if (source == "fontawesome") {
            x <- purrr::map(x, stringr::str_replace, "<svg", paste0('<svg fill="', fill, '" '))
        } else {
            x <- purrr::map(x, stringr::str_replace, "\\bcurrentColor\\b", fill)
        }
        x <- paste0(x, collapse = "\n")
        write(x, tmp)
    }

    leaflet::makeIcon(
        iconUrl = tmp,
        iconWidth = iconWidth,
        iconHeight = iconHeight
    )
}

#' Add custom markers to a leaflet map based on provided data.
#'
#' This function takes a leaflet map object, data containing marker information, and optional parameters to customize the map appearance.
#'
#' @param map A leaflet map object.
#' @param data Data frame containing information for markers. Should have columns like 'lon', 'lat', 'fill', 'icon', 'iconWidth', 'iconHeight', 'group', 'source', and 'solid'.
#' @param layers_control_position Position of the layers control on the map. Default is "bottomleft".
#' @param legend_position Position of the legend on the map. Default is "topright".
#'
#' @return A leaflet map object with added custom markers.
#'
#' @examples
#' map <- leaflet::leaflet() |>
#'     leaflet::addTiles() |>
#'     leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
#'
#' data <- centris_ca |>
#'     dplyr::rename(group = property_type) |>
#'     dplyr::mutate(
#'         iconWidth = 28,
#'         iconHeight = 28,
#'         icon = "map-marker"
#'     ) |>
#'     dplyr::left_join(
#'         tibble::tibble(
#'             group = unique(centris_ca$property_type),
#'             fill = wesanderson::wes_palette("Zissou1")[-1]
#'         ),
#'         by = "group"
#'     )
#'
#' map |>
#'     addCustomMarkers(data)
#'
#' data <- centris_ca |>
#'     dplyr::rename(group = property_type) |>
#'     dplyr::mutate(
#'         iconWidth = 28,
#'         iconHeight = 28
#'     ) |>
#'     dplyr::left_join(
#'         tibble::tibble(
#'             group = unique(centris_ca$property_type),
#'             fill = wesanderson::wes_palette("Zissou1")[-1],
#'             icon = c("building", "building", "home", "building")
#'         ),
#'         by = "group"
#'     )
#'
#' map |>
#'     addCustomMarkers(data)
#'
#' data <- centris_ca |>
#'     dplyr::rename(group = property_type) |>
#'     dplyr::mutate(
#'         iconWidth = 28,
#'         iconHeight = 28,
#'         source = "bootstrap"
#'     ) |>
#'     dplyr::left_join(
#'         tibble::tibble(
#'             group = unique(centris_ca$property_type),
#'             fill = wesanderson::wes_palette("Zissou1")[-1],
#'             icon = c("building", "building", "house-fill", "building")
#'         ),
#'         by = "group"
#'     )
#'
#' map |>
#'     addCustomMarkers(data, layers_control_position = FALSE)
#'
#' @export
addCustomMarkers <- function(map, data,
                             layers_control_position = "bottomleft",
                             legend_position = "topright") {
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
    if (!("source" %in% colnames(data))) {
        data$source <- "fontawesome"
    }
    if (!("solid" %in% colnames(data))) {
        data$solid <- TRUE
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
                    icon = ~ makeCustomIcon(
                        .x$icon[1],
                        .x$fill[1],
                        .x$iconWidth[1],
                        .x$iconHeight[1],
                        .x$source[1],
                        .x$solid[1]
                    )
                )
        })

    if (length(unique(data$group)) > 1 & is.character(legend_position)) {
        colors_labels <- data |>
            dplyr::distinct(group, fill)

        map <- map |>
            leaflet::addLegend(
                colors = colors_labels$fill,
                labels = colors_labels$group,
                position = legend_position
            )
    }

    if (length(unique(data$group)) > 1 & is.character(layers_control_position)) {
        map <- map |>
            leaflet::addLayersControl(
                overlayGroups = unique(data$group),
                position = layers_control_position
            )
    }

    return(map)
}
