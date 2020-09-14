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


#' generate_comparison_data
#' 
#' @description Generates comparison data, based on the supplied
#' parameters.
#' 
#' @param dat custom data format, produced by \code{\link{calculate_state_deuteration}}
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' 
#' @details This data is available in the GUI. 
#' All of the numerical values are rounded to 4 places after the dot!!
#' 
#' @importFrom dplyr rename
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_comparison_data

generate_comparison_data <- function(dat, 
                                     theoretical, 
                                     fractional,
                                     protein){
  if (theoretical){
    
    if (fractional){
      # theoretical & fractional
      dat %>%
        select(Protein, Sequence, State, Start, End, theo_frac_deut_uptake, err_theo_frac_deut_uptake) %>%
        mutate(theo_frac_deut_uptake  = round(theo_frac_deut_uptake , 4),
               err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Frac Exch" = theo_frac_deut_uptake , 
               "Err Theo Frac Exch" = err_theo_frac_deut_uptake)
      
    } else {
      # theoretical & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, theo_deut_uptake, err_theo_deut_uptake) %>%
        mutate(theo_deut_uptake = round(theo_deut_uptake, 4),
               err_theo_deut_uptake = round(err_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Abs Val Exch" = theo_deut_uptake,
               "Err Theo Abs Val Exch" = err_theo_deut_uptake)
    }
    
  } else {
    
    if (fractional){
      # experimental & fractional
      dat %>%
        select(Protein, Sequence, State, Start, End, frac_deut_uptake, err_frac_deut_uptake) %>%
        mutate(frac_deut_uptake = round(frac_deut_uptake, 4),
               err_frac_deut_uptake = round(err_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Frac Exch" = frac_deut_uptake,
               "Err Frac Exch" = err_frac_deut_uptake)
      
    } else {
      # experimental & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, deut_uptake, err_deut_uptake) %>%
        mutate(deut_uptake = round(deut_uptake, 4),
               err_deut_uptake = round(err_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Abs Val Exch" = deut_uptake,
               "Err Abs Val Exch" = err_deut_uptake)
      
    }
    
  }
  
}