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
