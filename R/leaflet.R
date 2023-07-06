#' @title Add color radius
#'
#' @param x the radius
#' @param radius all radius levels
#'
#' @export
leaflet_color_radius <- function(x, radius) {
    # pal <- RColorBrewer::brewer.pal(n = length(radius), name = "Dark2")
    # pal <- viridis::viridis(length(radius))
    # pal <- wesanderson::wes_palette("Zissou1", length(radius), type = "continuous")
    pal <- captal_palette(length(radius))
    pal[which(x == radius)]
}
leaflet_color_radius <- Vectorize(leaflet_color_radius, "x")

#' @title Basic leaflet from a reference point
#'
#' @param lat latitude
#' @param lng longitude
#' @param zoom map zoom (Default: 12.5)
#' @param provider https://rstudio.github.io/leaflet/basemaps.html
#'
#' @export
leaflet_basic <- function(lat, lng, zoom = 12.5, provider = NULL, marker = TRUE) {
    map <- tibble::tibble() |>
        leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE), width = "100%") |>
        leaflet::setView(
            lng = lng,
            lat = lat,
            zoom = zoom
        )

    if (isTRUE(marker)) {
        map <- map |>
            leaflet::addAwesomeMarkers(
                lng = lng,
                lat = lat,
                icon = leaflet::awesomeIcons(
                    icon = "ios-close",
                    iconColor = "black",
                    markerColor = "red",
                    library = "ion"
                )
            )
    }

    if (!is.null(provider)) {
        if (provider == "mapbox") {
            map <- map |>
                mapboxapi::addMapboxTiles(
                    style_id = "light-v9",
                    username = "mapbox",
                    access_token = Sys.getenv("mapbox_token")
                )
        } else {
            map <- map |>
                leaflet::addProviderTiles(provider)
        }
    } else {
        map <- map |>
            leaflet::addTiles()
    }

    return(map)
}

#' @title Add radius layer
#'
#' @param map leaflet map
#' @param radius the radius levels
#'
#' @export
leaflet_add_radius <- function(map, radius) {
    lat <- map[["x"]][["setView"]][[1]][[1]]
    lng <- map[["x"]][["setView"]][[1]][[2]]

    if (!is.vector(radius)) stop("O argumento radius deve ser um vetor.")
    if (length(radius) > 5) stop("É permitido no máximo 5 níveis de radius.")

    pal <- leaflet::colorFactor(
        palette = leaflet_color_radius(radius, radius),
        domain = stringr::str_c(radius, "km")
    )

    radius <- sort(radius)
    for (r in radius) {
        map <- map |>
            leaflet::addCircles(
                lng = lng,
                lat = lat,
                color = leaflet_color_radius(r, radius),
                radius = ~ r * 1000,
                weight = 5,
                fillOpacity = 0,
                stroke = TRUE
            )
    }

    map |>
        leaflet::addLegend(
            title = "Raios",
            position = "bottomleft",
            pal = pal,
            values = stringr::str_c(radius, "km")
        )
}

#' @title Add census layer
#'
#' @param map leaflet map
#' @param radius the radius levels
#' @param shape_census the geobr shape census
#'
#' @export
leaflet_add_census <- function(map, radius, shape_census) {
    lat <- map[["x"]][["setView"]][[1]][[1]]
    lng <- map[["x"]][["setView"]][[1]][[2]]
    # map <- leaflet_basic(input$lat, input$lng)

    if (!is.vector(radius)) stop("O argumento radius deve ser um vetor.")
    if (length(radius) > 5) stop("É permitido no máximo 5 níveis de radius.")

    pal <- leaflet::colorFactor(
        palette = leaflet_color_radius(radius, radius),
        domain = stringr::str_c(radius, "km")
    )

    index_list <- list()
    for (r in radius) {
        index <- sf::st_intersects(
            sf::st_point(x = c(lng, lat), dim = "XY") |>
                sf::st_sfc(crs = 4674) |>
                sf::st_buffer(dist = r * 1000),
            sf::st_transform(shape_census, crs = 4674)
        )
        index_list <- append(index_list, index)

        if (exists("shape_census_raio")) {
            shape_census_raio <- setdiff(shape_census[setdiff(tail(index_list, n = 1)[[1]], unlist(index_list[-length(index_list)])), ], shape_census_raio)
        } else {
            shape_census_raio <- shape_census[index_list[[1]], ]
        }

        # Warning message:
        # sf layer has inconsistent datum (+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs).
        # Need '+proj=longlat +datum=WGS84'
        suppressWarnings({
            map <- map |>
                leaflet::addPolygons(
                    data = shape_census_raio,
                    color = leaflet_color_radius(r, radius)
                )
        })
    }

    map |>
        leaflet::addLegend(
            title = "Setores Censitários",
            position = "bottomright",
            pal = pal,
            values = stringr::str_c(radius, "km")
        )
}

#' @title Add city layer
#'
#' @param map leaflet map
#' @param shape_city the geobr shape city to add stroke in city
#'
#' @export
leaflet_add_stroke_city <- function(map, shape_city) {
    suppressWarnings({
        map |>
            leaflet::addPolygons(
                data = shape_city,
                color = "#03BF1F",
                fill = NA
            )
    })
}
