#'
#'
#' @export plot_aggregated_uptake

plot_aggregated_uptake <- function(aggregated_dat,
                                   fractional = T,
                                   time_100 = max(unique(aggregated_dat[["Exposure"]])),
                                   panels = T,
                                   theoretical = F){

  aggregated_dat <- as.data.table(aggregated_dat)

  if(fractional){

    aggregated_dat <- aggregated_dat[Exposure < time_100,]

    plt <- ggplot(aggregated_dat) +
      geom_tile(aes(x = position, y = as.factor(Exposure), fill = frac_uc)) +
      scale_fill_gradient2(low = "white", high = "red") +
      theme(legend.position = "bottom") +
      labs(y = "Exposure",
           x = "Position",
           fill = "Frac DU")

    if(panels){

      limit = 50
      length = max(aggregated_dat[["position"]])
      n_panels = ceiling(length/limit)
      panels <- NULL

      x <- lapply(1:n_panels, function(i){

        plt_tmp <- plt +
          coord_cartesian(xlim = c((i-1)* 50, i*50))

        if(i == n_panels){
          plt <- plt_tmp + labs(x = "Position")
        }

        plt_tmp

      })

      plt <- do.call(gridExtra::grid.arrange, x)
    }

  }

  return(plt)

}

