#' generate_kinetic_data
#' 
#' @description Generates deuterium uptake data, based on the supplied
#' parameters.
#' 
#' @param dat custom format, produced by 
#' \code{\link{generate_kinetic_data_set}}
#' @param theoretical \code{logical}, determines if plot shows theoretical values
#' @param fractional \code{logical}, determines if plot shows fractional values
#' 
#' @details This data is available in the GUI. 
#' All of the numerical values are rounded to 4 places after the dot!!
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_kinetic_data

generate_kinetic_data <- function(dat, 
                                  theoretical, 
                                  fractional){
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional  
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, theo_frac_deut_uptake, err_avg_theo_in_time) %>%
        mutate(theo_frac_deut_uptake = round(theo_frac_deut_uptake, 4), 
               err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4)) %>%
        rename("Time Point" = time_chosen,
               "Theo Frac Exch" = theo_frac_deut_uptake,
               "Theo Err Frac Exch" = err_theo_frac_deut_uptake)
      
    } else {
      # theoretical & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, theo_deut_uptake, err_theo_deut_uptake) %>%
        mutate(theo_deut_uptake = round(theo_deut_uptake, 4), 
               err_theo_deut_uptake = round(err_theo_deut_uptake, 4)) %>%
        rename("Time Point" = time_chosen,
               "Theo Abs Val Exch" = theo_deut_uptake,
               "Theo Err Abs Val Exch" = err_theo_deut_uptake)
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, frac_deut_uptake, err_frac_deut_uptake) %>%
        mutate(frac_deut_uptake = round(frac_deut_uptake, 4), 
               err_frac_deut_uptake = round(err_frac_deut_uptake, 4)) %>%
        rename("Time Point" = time_chosen,
               "Frac Exch" = frac_deut_uptake,
               "Err Frac Exch" = err_frac_deut_uptake)
      
    } else {
      # experimental & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, deut_uptake, err_deut_uptake) %>%
        mutate(deut_uptake = round(deut_uptake, 4), 
               err_deut_uptake = round(err_deut_uptake, 4)) %>%
        rename("Time Point" = time_chosen,
               "Abs Val Exch" = deut_uptake,
               "Err Abs Val Exch" = err_deut_uptake)
    }
    
  }
  
}