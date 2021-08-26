#' HaDeX customized ggplot theme
#'
#' @description This function HaDeXifies plot. It adds HaDeX logo and ggplot theme.
#'
#' @param plt ggplot object. Plot to HaDeXify.
#'
#' @importFrom magick image_read
#' @importFrom ggplot2 element_text annotation_custom
#'
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- generate_differential_data_set(dat)
#' HaDeXify(plot_differential(diff_uptake_dat))
#'
#' @export

HaDeXify <- function(plt) {

  bitmap <- readRDS(system.file(package = "HaDeX",
                                "HaDeX/www/HaDeX_logo.RDS"))
  img <- image_read(bitmap)

  suppressMessages({
    plt  +
      theme(text = element_text(family="Lato"),
            legend.position="bottom") +
      annotation_custom(grid::rasterGrob(img, interpolate = TRUE,
                                         height = 0.05,
                                         x = 0.99, y = 0.01,
                                         hjust = 1, vjust = 0))
  })
}
