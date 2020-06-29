#' generate_comparison_plot
#' 
#' @description Generates comparison plot based on supplied data
#' and parameters: theoretical/relative
#' 
#' @param dat custom data format, produced by \code{\link{calculate_state_deuteration}}
#' @param theoretical ...
#' @param relative ...
#' 
#' @details This plot is visible in GUI. The names of the parameters
#' will be changed later after the glossary project.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_comparison_plot

generate_comparison_plot <- function(dat, 
                                     theoretical, 
                                     relative){
  
  if (theoretical) {
    
    if (relative) {
      # theoretical & relative
      ggplot(data = dat) +
        geom_segment(data = dat, aes(x = Start, y = avg_theo_in_time, xend = End, yend = avg_theo_in_time, color = State)) +
        geom_errorbar(data = dat, aes(x = Med_Sequence, ymin = avg_theo_in_time - err_avg_theo_in_time, ymax = avg_theo_in_time + err_avg_theo_in_time, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(breaks = seq(-200, 200, 10), expand = c(0, 0))
      
    } else {
      # theoretical & absolute
      ggplot(data = dat) +
        geom_segment(data = dat, aes(x = Start, y = abs_avg_theo_in_time, xend = End, yend = abs_avg_theo_in_time, color = State)) +
        geom_errorbar(data = dat, aes(x = Med_Sequence, ymin = abs_avg_theo_in_time - err_abs_avg_theo_in_time, ymax = abs_avg_theo_in_time + err_abs_avg_theo_in_time, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(expand = c(0, 0))
    } 
    
  } else {
    
    if (relative) {
      # experimantal & relative
      ggplot(data = dat) +
        geom_segment(data = dat, aes(x = Start, y = frac_exch_state, xend = End, yend = frac_exch_state, color = State)) +
        geom_errorbar(data = dat, aes(x = Med_Sequence, ymin = frac_exch_state - err_frac_exch_state, ymax = frac_exch_state + err_frac_exch_state, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(breaks = seq(-200, 200, 10), expand = c(0, 0))
      
    } else {
      # experimental & absolute 
      ggplot(data = dat) +
        geom_segment(data = dat, aes(x = Start, y = abs_frac_exch_state, xend = End, yend = abs_frac_exch_state, color = State)) +
        geom_errorbar(data = dat, aes(x = Med_Sequence, ymin = abs_frac_exch_state - err_abs_frac_exch_state, ymax = abs_frac_exch_state + err_abs_frac_exch_state, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(expand = c(0, 0))
    }
    
  }
  
}