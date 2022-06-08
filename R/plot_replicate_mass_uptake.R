#' Plots the mass uptake curve from the replicates
#'
#' @param dat ...
#' @param protein ...
#' @param state ...
#' @param sequence ...
#' @param show_aggregated ...
#'
#' @seealso 
#' \code{\link{read_hdx}}
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
                                       show_aggregated = FALSE){
  
  
  if(show_aggregated) {
    
    dat %>%
      filter(Protein == protein, 
             State == state,
             Sequence == sequence,
             Exposure > 0.001) %>%
      calculate_exp_masses_per_replicate() %>%
      mutate(mass_uptake = avg_exp_mass - MHP) %>%
      ggplot() + 
      geom_point(aes(x = Exposure, y = mass_uptake)) +
      scale_x_log10() +
      labs(x = "Exposure",
           y = "Mass uptake") +
      coord_cartesian(ylim = c(0, NA))
    
  } else {
    
    dat %>%
      filter(Protein == protein, 
             State == state,
             Sequence == sequence,
             Exposure > 0.001) %>%
      mutate(exp_mass = Center*z - z*1.00727647,
             weighted_Inten = scale(Inten),
             mass_uptake = exp_mass - MHP) %>%
      ggplot() +
      geom_point(aes(x = Exposure, y = mass_uptake, colour = as.factor(z))) +
      scale_x_log10() +
      labs(x = "Exposure",
           y = "Mass uptake",
           colour = "Charge") +
      coord_cartesian(ylim = c(0, NA)) + 
      theme(legend.position = "bottom")
      
  }
  
  
  
}