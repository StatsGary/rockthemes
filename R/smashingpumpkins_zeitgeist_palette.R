smashpump_palette <- c(
    "#963A82",
    "#0EAB4A",
    "#474095",
    "#40A7C5",
    "#149357",
    "#E2E37D",
    "#213980",
    "#D23522",
    "#328D71",
    "#E85733"
)

#' @title Smashing Pumpkins Zeitgeist Palette
#' @description A colour palette for the Smashing Pumpkins Zeitgeist palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname smashpump_pal
#' @examples
#' library(scales)
#' show_col(smashpump_pal()(10))
#' @export
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

smashpump_pal <- function(n, type = c("discrete", "continuous"),
                                reverse = FALSE){
    smashpump <- smashpump_palette

    if (reverse == TRUE) {
        smashpump <- rev(smashpump)
    }

    if (missing(n)) {
        n <- length(smashpump)
    }

    type <- match.arg(type)

    if (type == "discrete" && n > length(smashpump)) {
        stop(glue::glue("Palette does not have {n} colors, maximum is {length(smashpump)}!"))
    }

    smashpump <- switch(type,
                              continuous = grDevices::colorRampPalette(smashpump)(n),
                              discrete = smashpump[1:n])

    smashpump <- scales::manual_pal(smashpump)

    return(smashpump)
}

#' @title scale_color_smashpump
#' @rdname smashpump_pal
#' @export
#' @examples
#'
#'library(ggplot2)
#'ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'     scale_color_smashpump()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_smashpump <- function(n, type = "discrete",
                                        reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("color", "smashpump",
                                smashpump_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_color_gradientn(colors = smashpump_pal(n = n, type = type,
                                                                    reverse = reverse)(8))
    }
}

#' @title scale_colour_smashpump
#' @rdname smashpump_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_smashpump()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_smashpump <- scale_color_smashpump

#' @title scale_fill_smashpump
#' @rdname smashpump_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'     geom_histogram(aes(fill = class),
#'                    col = "black", size = 0.1) +
#'     scale_fill_smashpump()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_smashpump <- function(n, type = "discrete",
                                       reverse = FALSE, ...){
    if (type == "discrete") {
        ggplot2::discrete_scale("fill", "smashpump",
                                smashpump_pal(n = n, type = type,
                                                    reverse = reverse), ...)
    } else {
        ggplot2::scale_fill_gradientn(colors = smashpump_pal(n = n, type = type,
                                                                   reverse = reverse)(8))
    }
}
