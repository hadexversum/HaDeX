#' Plots the mass uptake curve from the replicates
#'
#' @description ...
#' 
#' @param dat ...
#' @param protein ...
#' @param state ...
#' @param sequence ...
#' @param show_aggregated ...
#' @param log_x ...
#' 
#' @details ...
#'
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_exp_masses_per_replicate}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' plot_replicate_mass_uptake(dat)
#' 
#' @export plot_replicate_mass_uptake

plot_replicate_mass_uptake <- function(dat, 
                                       protein = dat[["Protein"]][1],
                                       state = dat[["State"]][1],
                                       sequence = dat[["Sequence"]][1],
                                       show_aggregated = FALSE,
                                       log_x = TRUE){
  
  
  if(show_aggregated) {
    
    mass_uptake_plot <- dat %>%
      filter(Protein == protein, 
             State == state,
             Sequence == sequence,
             Exposure > 0.001) %>%
      calculate_exp_masses_per_replicate() %>%
      mutate(mass_uptake = avg_exp_mass - MHP) %>%
      ggplot() + 
      geom_point(aes(x = Exposure, y = mass_uptake)) 
      
  } else {
    
    mass_uptake_plot <- dat %>%
      filter(Protein == protein, 
             State == state,
             Sequence == sequence,
             Exposure > 0.001) %>%
      mutate(exp_mass = Center*z - z*1.00727647,
             weighted_Inten = scale(Inten),
             mass_uptake = exp_mass - MHP) %>%
      ggplot() +
      geom_point(aes(x = Exposure, y = mass_uptake, colour = as.factor(z))) +
      labs(colour = "Charge") +
      theme(legend.position = "bottom")
      
  }
  
  mass_uptake_plot <- mass_uptake_plot +
    labs(x = "Time points [min]",
         y = "Mass uptake [Da]") +
    coord_cartesian(ylim = c(0, NA))
    
  if(log_x){ mass_uptake_plot <- mass_uptake_plot + scale_x_log10() }
  
  return(HaDeXify(mass_uptake_plot))
  
}