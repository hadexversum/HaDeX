#' HaDeX customized ggplot theme
#' @description This function HaDeXifies plot. It adds HaDeX logo and ggplot theme.
#' @param plt ggplot object. Plot to HaDeXify.
#' @importFrom magick image_read
#' @importFrom ggplot2 element_text
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- generate_differential_data_set(dat)
#' HaDeXify(plot_differential(diff_uptake_dat))
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
                                         width=unit(2.5,'cm'),
                                         x = unit(1,"npc"), y = unit(-1.8,"cm"),
                                         hjust = 1, vjust=0)) +
      coord_cartesian(clip = "off")
  })
}
