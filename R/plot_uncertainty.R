#' Plot the uncertainty of the measurement for peptides
#'
#' @description Plot the uncertainty of the mass measurements - for aggregated
#' data or before aggregation - to see if there is a region with uncertainty 
#' higher than acceptable. 
#' 
#' @param dat ...
#' @param protein ...
#' @param state ...
#' @param aggregated ...
#' @param separate_times ....
#' @param show_threshold ...
#' 
#' @details ...
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso
#' \code{\link{read_hdx}}
#' \code{\link{calculate_exp_masses}} 
#'  
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' plot_uncertainty(dat)
#'
#' @export plot_uncertainty 

plot_uncertainty <- function(dat, 
                             protein = dat[["Protein"]][1],
                             state = dat[["State"]][1],
                             aggregated = TRUE,
                             separate_times = TRUE, 
                             show_threshold = TRUE){
  
  dat <- dat %>%
    filter(Protein == protein,
           State == state)
  
  if(aggregated){
    
    plot_dat <- calculate_exp_masses(dat) 
    
  } else {
    
    plot_dat <- dat %>% 
      mutate(exp_mass = Center*z - z*1.00727647) %>%
      select(-Inten, -Center,  -MaxUptake, -z) %>%
      group_by(Protein, Sequence, Start, End, MHP, State, Exposure) %>%
      mutate(err_avg_mass = sd(exp_mass)) %>%
      select(-exp_mass) %>%
      unique(.)
  }
  
  uncertainty_plot <- plot_dat %>%
    ggplot() +
    geom_segment(aes(x = Start, xend = End, y = err_avg_mass, yend = err_avg_mass, color = as.factor(Exposure))) +
    labs(x = "Peptide position",
         y = "Uncertainy(mass)",
         color = "Exposure") +
    theme(legend.position = "bottom")
  
  if(show_threshold){
    
    uncertainty_plot <- uncertainty_plot + 
      geom_hline(yintercept = 1, linetype = "dashed", size = 0.5,  color = "red", alpha = 0.5) 
  }
  
  if(separate_times){
    
    uncertainty_plot <- uncertainty_plot + 
      facet_wrap(~ Exposure) 
  }
  
  return(HaDeXify(uncertainty_plot))

  }