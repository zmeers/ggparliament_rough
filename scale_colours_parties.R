## Functions to create political party colour palettes for ggparliament
## Zoe Meers

#' all colours
party_cols <- c(
    `light blue` = "#009de3",
    `dark blue` = "#1c396e",
    `red` = "#ed1b35",
    `light grey` = "#cccccc",
    `dark grey` = "#8c8c8c",
    `black` = "#000000",
    `green` = "#009900",
    `yellow` = "#FFFF00",
    `maroon`= "#800000",
    `navy`= "#000080",
    `purple`= "#800080",
    `orange`= "#FF5733"
)

#' Function to extract party colours as hex codes
#'
#' @param ... Character names of party_colours
#' @author
#' Zoe Meers
party_colours <- function(...) {
    cols <- c(...)
    
    if (is.null(cols)) {
        return(party_cols)
    }
    
    party_cols[cols]
}

#' Political party palettes
#' @description
#' This list creates palette types.
#' @author
#' Zoe Meers
#'
party_pal <- list(
    `GER` = party_colours("red", "grey", "green", "dark blue"),
    `AUS` = party_colours("red", "grey", "green", "dark blue"),
    `USA` = party_colours("red",  "dark blue", "dark grey"),
    `FRA` = party_colours("red", "maroon", "yellow", "orange", "light blue", "dark blue", "dark grey"),
    `UK` = party_colours("light blue", "purple","dark grey", "purple","green","light grey","yellow","red")
)

#' Return function to interpolate a party colour palette
#'
#' @param palette Character name of palette in party_pal (i.e. UK, AUS, USA)
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#' @author
#' Zoe Meers
#'

party_palettes <- function(palette = "AUS", reverse = FALSE, ...) {
    pal <- party_pal[[palette]]
    
    if (reverse) pal <- rev(pal)
    
    colorRampPalette(pal, ...)
}

#' colour scale constructor for party colours
#'
#' @param palette Character name of palette in party_pal (i.e. AUS, GER, USA)
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_colour_gradientn(), used respectively when discrete is TRUE or FALSE
#' @examples
#' Colour by discrete variable using default palette
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, colour = Species)) + geom_point(size = 4) + scale_colour_party()
#' @author
#' Zoe Meers

scale_colour_party <- function(palette = "AUS", discrete = TRUE, reverse = FALSE, ...) {
    pal <- party_palettes(palette = palette, reverse = reverse)
    
    if (discrete) {
        discrete_scale("colour", paste0("party_", palette), palette = pal, ...)
    } else {
        scale_colour_gradientn(colours = pal(256), ...)
    }
}

#' Fill scale constructor for party colours
#'
#' @param palette Character name of palette in party_pal (i.e. UK, FRA, AUS, GER)
#' @param discrete Boolean indicating whether colour aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradient(), used respectively when discrete is TRUE or FALSE
#' @author
#' Zoe Meers
scale_fill_party <- function(palette = "AUS", discrete = TRUE, reverse = FALSE, ...) {
    pal <- party_palettes(palette = palette, reverse = reverse)
    
    if (discrete) {
        discrete_scale("fill", paste0("party_", palette), palette = pal, ...)
    } else {
        scale_colour_gradientn(colours = pal(256), ...)
    }
}
