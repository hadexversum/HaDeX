#' HaDeX customized ggplot theme
#'
#' @param plt plot
#' @importFrom magick image_read
#' @importFrom cowplot draw_image ggdraw draw_plot
#' @export

HaDeXify <- function(plt) {

  bitmap <- readRDS("./inst/HaDeX/www/HaDeX_logo.RDS")
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

