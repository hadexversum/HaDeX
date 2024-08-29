#'
#' @examples 
#' kin_dat <- create_uptake_dataset(alpha_dat, states = "Alpha_KSCN" )
#' aggregated_dat <- create_aggregated_uptake_dataset(kin_dat)
#' plot_aggregated_uptake(aggregated_dat, panels = F)
#' plot_aggregated_uptake(aggregated_dat, fractional = F, panels = F)
#' plot_aggregated_uptake(aggregated_dat, fractional = F, theoretical = T, panels = T)
#'
#' @export
plot_aggregated_uptake <- function(aggregated_dat,
                                   fractional = TRUE,
                                   theoretical = FALSE,
                                   interactive = FALSE,
                                   time_100 = max(unique(aggregated_dat[["Exposure"]])),
                                   panels = TRUE){

  aggregated_dat <- as.data.table(aggregated_dat)

  if(fractional){

    fill_value <- "frac_deut_uptake"
    err_value <- "err_frac_deut_uptake"
    title <- "Fractional deuterium uptake"
    fill_title <- "Fractional DU [%]"
    
    if(theoretical){
      
      fill_value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      title <- "Theoretical fractional deuterium uptake"
      fill_title <- "Theoretical Fractional DU [%]"
      
    }
    
  } else {
    
    fill_value <- "deut_uptake"
    err_value <- "err_deut_uptake"
    title <- "Deuterium uptake"
    fill_title <- "DU [Da]"
    
    if(theoretical){
      
      fill_value <- "theo_deut_uptake"
      err_value <- "err_theo_deut_uptake"
      title <- "Theoretical deuterium uptake"
      fill_title <- "Theoretical DU [Da]"
    }
    
  }

  aggregated_dat <- aggregated_dat[Exposure < time_100,]
  
  plt <- ggplot(aggregated_dat) +
    geom_tile(aes(x = position, y = as.factor(Exposure), fill = get(fill_value))) +
    scale_fill_gradient2(low = "white", high = "red") +
    theme(legend.position = "bottom") +
    labs(y = "Exposure",
         x = "Position",
         fill = fill_title)
  
  if(panels){
    
    limit = 50
    length = max(aggregated_dat[["position"]])
    n_panels = ceiling(length/limit)
    panels <- NULL
    
    x <- lapply(1:n_panels, function(i){
      
      plt_tmp <- plt +
        coord_cartesian(xlim = c((i-1)* 50, i*50)) +
        theme(legend.position = "none") +
        labs(x = "")
      
      if(i == n_panels){
        plt_tmp <- plt_tmp + labs(x = "Position")
      }
      
      plt_tmp
      
    })
    
    plt <- do.call(gridExtra::grid.arrange, c(x, ncol = 1))
  }
  
  
  return(HaDeXify(plt))

}

