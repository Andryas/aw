#' @title aw_palette
#'
#' @description
#' Bunch of palette that I think it is awesome
#'
#' @noRd

#' @title aw_palettes
#'
#' @description
#' Bunch of palette that I think it is awesome
#'
#' Use \code{\link{aw_palette}} to construct palettes of desired length.
#'
#' @export
aw_palettes <- list(
    GreenToRed = c("#C85C5C", "#F9975D", "#FBD148", "#B2EA70")
)

#' @title A AW palette generator
#'
#' @param n Number of colors desired.
#' @param name Name of desired palette. Choices are:
#'   \code{green_to_red}
#' @param type Either "continuous" or "discrete". Use continuous if you want
#'   to automatically interpolate between colours.
#'   @importFrom graphics rgb rect par image text
aw_palette <- function(name, n, type = c("discrete", "continuous")) {
    type <- match.arg(type)

    pal <- aw_palettes[[name]]
    if (is.null(pal)) {
        stop("Palette not found.")
    }

    if (missing(n)) {
        n <- length(pal)
    }

    if (type == "discrete" && n > length(pal)) {
        stop("Number of requested colors greater than what palette can offer")
    }

    out <- switch(type,
        continuous = grDevices::colorRampPalette(pal)(n),
        discrete = pal[1:n]
    )
    structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
    n <- length(x)
    old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
    on.exit(par(old))

    image(1:n, 1, as.matrix(1:n),
        col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n"
    )

    rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
    text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

#' Numeric Color Ramp Function
#'
#' This function generates a color ramp based on numeric input values.
#'
#' @param x A numeric vector representing the values to be color-mapped.
#' @param colors A vector of three color names defining the gradient.
#'               The default is c("red", "white", "darkgreen").
#' @return A character vector of RGB color values based on the input numeric vector.
#' @examples
#'
#' data <- c(1, 2, 3, 4, 5)
#' colors <- colorRampNumeric(data)
#' plot(data, col = colors, pch = 16, cex = 2)
#'
#' @export
colorRampNumeric <- function(x, colors = c("red", "white", "darkgreen")) {
    cr <- colorRamp(colors)
    crm <- cr((x - min(x)) / (max(x) - min(x)))
    crm <- apply(crm, 2, as.integer)
    rgb(crm[, 1], crm[, 2], crm[, 3], maxColorValue = 255)
}
