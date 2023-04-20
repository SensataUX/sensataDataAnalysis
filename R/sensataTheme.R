#' theme_sensata: Custom Theme for Sensata
#'
#' A custom theme for Sensata with custom color palette and fonts for ggplot2.
#'
#' @return A ggplot2 theme object
#' @export
#' @examples
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   theme_sensata()
#' @seealso \code{\link[ggplot2]{theme}}
#' @seealso \code{\link[showtext]{showtext_auto}}
#' @seealso \code{\link[sysfonts]{font_add_google}}
theme_sensata <- function() {

  # Create ggplot2 theme
  theme_sensata <- theme(
    text = element_text(family = "Open Sans"),
    axis.title = element_text(colour = "dimgray"),
    axis.text.x = element_text(colour = "dimgray"),
    axis.text.y = element_text(colour = "dimgray"),
    legend.text = element_text(colour = "dimgray"),
    legend.title = element_text(colour = "dimgray"),
    plot.title = element_text(family = "Montserrat",
                              colour = "black",
                              lineheight=1),
    plot.caption = element_text(colour = "dimgray"),
    plot.subtitle = element_text(family = "Montserrat"),
    panel.background = element_rect(linewidth = 0, fill = "white"),
    panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',
                                    colour = "gray85"),
    panel.grid.major.x = element_blank(),
    legend.key=element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white"),
    panel.border = element_blank(),
    # Add any additional customizations here
    # ...
  )

  return(theme_sensata)
}

#' Palette for Sensata Theme
#'
#' A custom color palette for Sensata theme.
#'
#' @return A vector of colors
#' @export
#' @examples
#' my_palette <- sensata_palette()
#' my_palette
sensata_palette <- function(palette = c("#003f5c",
                                        "#58508d",
                                        "#bc5090",
                                        "#ff6361",
                                        "#ffa600",
                                        "#C0C0C0")) {
  sensata_palette <- palette
  return(sensata_palette)
}
