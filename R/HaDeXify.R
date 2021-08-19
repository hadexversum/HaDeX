#' HaDeX customized ggplot theme
#'
#' @param plt plot
#'
#' @export

HaDeXify <- function(plt) {

  bitmap <- readRDS("./inst/HaDeX/www/HaDeX_logo.RDS")
  img <- image_read(bitmap)

  cowplot::ggdraw() +
    cowplot::draw_plot(
      plt +
        theme(text = element_text(size=16, family="Lato"),
              legend.position="bottom")) +
    cowplot::draw_image(img, scale = 0.1, x = 0.44, y = -0.46)
}

