#' bay colour palette
#'
#' Provides a colour scheme expanded from the
#' \href{https://github.com/jakelawlor/PNWColors}{PNWColors - Bay} color pallette.
#'
#' @param num_values Number of values to colour. If more than 9, colours start
#'   to repeat.
#'
#' @examples
#' if (requireNamespace("scales", quietly = TRUE)) {
#'   library(scales)
#'   show_col(bay_palette(9))
#' }
#' @export
bay_palette <- function(num_values) {
  
  bay_colors <- c(indigo = "#00496F",
                  teal = "#0F85A0",
                        green = "#6B9555",
                        citron = "#ACB64E",
                        yellow = "#edd746",
                        orange = "#E58C35",
                        flame = "#E1672D",
                        red = "#BD3922",
                        maroon = "#903B3D",
                        orchid = "#623D57")
  
  color_order <- c("teal","green", "flame",  "red", "orchid","indigo", "yellow", "maroon", "citron", "orange")
  
  num_colors <- length(color_order)
  if (num_values < num_colors) {
    unname(bay_colors[Filter(
      function(color) color %in% color_order[1:num_values],
      names(bay_colors)
    )])
  } else {
    color_indeces <- 0:(num_values - 1) %% num_colors
    unname(bay_colors[color_indeces + 1])
  }
  
}


#' bay colour palette for ggplot2
#'
#' Provides a colour scheme based on the
#' \href{http://ethanschoonover.com/solarized}{solarized} accent colours for use
#' with ggplot2.
#'
#' @param ... Arguments passed on to discrete_scale to control name, limits,
#'   breaks, labels and so forth.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = hp, colour = factor(cyl))) +
#'   geom_point() +
#'   scale_colour_bay()
#' @export
scale_colour_bay <- function(...) {
  ggplot2::discrete_scale("colour", "bay", bay_palette, ...)
}

#' @rdname scale_colour_bay
#' @export
scale_color_bay <- function(...) {
  scale_colour_bay(...)
}

#' @rdname scale_colour_bay
#' @export
scale_fill_bay <- function(...) {
  ggplot2::discrete_scale("fill", "bay", bay_palette, ...)
}
