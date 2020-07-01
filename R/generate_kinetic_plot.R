#' generate_kinetic_plot
#' 
#' @description Generates deuterium uptake curve based on supplied data
#' and parameters.
#' 
#' @param dat produced by \code{\link{generate_kinetic_data_set}} function
#' @param theoretical ...
#' @param relative ...
#' 
#' @details This plot is visible in GUI. The names of the parameters
#' and variables will be changed later after the glossary project.
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
        ggplot(aes(x = time_chosen, y = avg_theo_in_time, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = avg_theo_in_time - err_avg_theo_in_time, ymax = avg_theo_in_time + err_avg_theo_in_time, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop))
      
    } else {
      # theoretical & absolute
      dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = abs_avg_theo_in_time, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = abs_avg_theo_in_time - err_abs_avg_theo_in_time, ymax = abs_avg_theo_in_time + err_abs_avg_theo_in_time, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop))
      
    }
    
  } else {
    
    if(relative){
      # experimental & relative
      dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = frac_exch_state, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = frac_exch_state - err_frac_exch_state, ymax = frac_exch_state + err_frac_exch_state, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop))
      
    } else {
      # experimental & absolute
      dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = abs_frac_exch_state, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = abs_frac_exch_state - err_abs_frac_exch_state, ymax = abs_frac_exch_state + err_abs_frac_exch_state, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop))
      
    }
    
  }
  
}
