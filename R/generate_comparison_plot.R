#' generate_comparison_plot
#' 
#' @description Generates comparison plot based on supplied data
#' and parameters.
#' 
#' @param dat produced by \code{\link{calculate_state_deuteration}} function
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_comparison_plot

generate_comparison_plot <- function(dat, 
                                     theoretical, 
                                     fractional){
  
  if (theoretical) {
    
    if (fractional) {
      # theoretical & fractional
      ggplot(data = dat) +
        geom_segment(data = dat, aes(x = Start, y = theo_frac_deut_uptake, xend = End, yend = theo_frac_deut_uptake, color = State)) +
        geom_errorbar(data = dat, aes(x = Med_Sequence, ymin = theo_frac_deut_uptake - err_theo_frac_deut_uptake, ymax = theo_frac_deut_uptake + err_theo_frac_deut_uptake, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(breaks = seq(-200, 200, 10), expand = c(0, 0))
      
    } else {
      # theoretical & absolute
      ggplot(data = dat) +
        geom_segment(data = dat, aes(x = Start, y = theo_deut_uptake, xend = End, yend = theo_deut_uptake, color = State)) +
        geom_errorbar(data = dat, aes(x = Med_Sequence, ymin = theo_deut_uptake - err_theo_deut_uptake, ymax = theo_deut_uptake + err_theo_deut_uptake, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(expand = c(0, 0))
    } 
    
  } else {
    
    if (fractional) {
      # experimantal & fractional
      ggplot(data = dat) +
        geom_segment(data = dat, aes(x = Start, y = frac_deut_uptake, xend = End, yend = frac_deut_uptake, color = State)) +
        geom_errorbar(data = dat, aes(x = Med_Sequence, ymin = frac_deut_uptake - err_frac_deut_uptake, ymax = frac_deut_uptake + err_frac_deut_uptake, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(breaks = seq(-200, 200, 10), expand = c(0, 0))
      
    } else {
      # experimental & absolute 
      ggplot(data = dat) +
        geom_segment(data = dat, aes(x = Start, y = deut_uptake, xend = End, yend = deut_uptake, color = State)) +
        geom_errorbar(data = dat, aes(x = Med_Sequence, ymin = deut_uptake - err_deut_uptake, ymax = deut_uptake + err_deut_uptake, color = State)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(expand = c(0, 0))
    }
    
  }
  
}