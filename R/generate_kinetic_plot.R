#' generate_kinetic_plot
#' 
#' @description Generates deuterium uptake curve based on supplied data
#' and parameters.
#' 
#' @param dat produced by \code{\link{generate_kinetic_data_set}} function
#' @param theoretical ...
#' @param relative ...
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_kinetic_plot

generate_kinetic_plot <- function(dat, 
                                  theoretical, 
                                  relative){
  
  if(theoretical){
    
    if(relative){
      # theoretical & relative  
      dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = theo_frac_deut_uptake, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = theo_frac_deut_uptake - err_theo_frac_deut_uptake, ymax = theo_frac_deut_uptake + err_theo_frac_deut_uptake, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop))
      
    } else {
      # theoretical & absolute
      dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = theo_deut_uptake, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = theo_deut_uptake - err_theo_deut_uptake, ymax = theo_deut_uptake + err_theo_deut_uptake, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop))
      
    }
    
  } else {
    
    if(relative){
      # experimental & relative
      dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = frac_deut_uptake, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = frac_deut_uptake - err_frac_deut_uptake, ymax = frac_deut_uptake + err_frac_deut_uptake, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop))
      
    } else {
      # experimental & absolute
      dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = deut_uptake, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = deut_uptake - err_deut_uptake, ymax = deut_uptake + err_deut_uptake, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop))
      
    }
    
  }
  
}
