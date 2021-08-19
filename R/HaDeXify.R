#' HaDeX customized ggplot theme
#' @description This function HaDeXifies plot. It adds HaDeX logo and ggplot theme.
#' @param plt ggplot object. Plot to HaDeXify.
#' @importFrom magick image_read
#' @importFrom cowplot draw_image ggdraw draw_plot
#' @importFrom ggplot2 element_text
#' @export

HaDeXify <- function(plt) {

  bitmap <- readRDS(system.file(package = "HaDeX",
                                "HaDeX/www/HaDeX_logo.RDS"))
  img <- image_read(bitmap)

  suppressWarnings({
    ggdraw() +
      draw_plot(
        plt +
          theme(text = element_text(size=16, family="Lato"),
                legend.position="bottom")) +
      draw_image(img, scale = 0.1, x = 0.44, y = -0.46)
  })
}

