#' generate_kinetic_data
#' 
#' @description Generates deuterium uptake data, based on the supplied
#' parameters.
#' 
#' @param dat custom format, produced by 
#' \code{\link{generate_kinetic_data_set}}
#' @param theoretical ...
#' @param relative ...
#' 
#' @details This data is available in the GUI. The names of the parameters
#' and variables will be changed later after the glossary project.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_kinetic_data

generate_kinetic_data <- function(dat, 
                                  theoretical, 
                                  relative){
  
  if(theoretical){
    
    if(relative){
      # theoretical & relative  
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, avg_theo_in_time, err_avg_theo_in_time) %>%
        mutate(avg_theo_in_time = round(avg_theo_in_time, 4), 
               err_avg_theo_in_time = round(err_avg_theo_in_time, 4)) %>%
        rename("Time Point" = time_chosen,
               "Theo Frac Exch" = avg_theo_in_time,
               "Theo Err Frac Exch" = err_avg_theo_in_time)
      
    } else {
      # theoretical & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, abs_avg_theo_in_time, err_abs_avg_theo_in_time) %>%
        mutate(abs_avg_theo_in_time = round(abs_avg_theo_in_time, 4), 
               err_abs_avg_theo_in_time = round(err_abs_avg_theo_in_time, 4)) %>%
        rename("Time Point" = time_chosen,
               "Theo Abs Val Exch" = abs_avg_theo_in_time,
               "Theo Err Abs Val Exch" = err_abs_avg_theo_in_time)
    }
    
  } else {
    
    if(relative){
      # experimental & relative
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, frac_exch_state, err_frac_exch_state) %>%
        mutate(frac_exch_state = round(frac_exch_state, 4), 
               err_frac_exch_state = round(err_frac_exch_state, 4)) %>%
        rename("Time Point" = time_chosen,
               "Frac Exch" = frac_exch_state,
               "Err Frac Exch" = err_frac_exch_state)
      
    } else {
      # experimental & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, abs_frac_exch_state, err_abs_frac_exch_state) %>%
        mutate(abs_frac_exch_state = round(abs_frac_exch_state, 4), 
               err_abs_frac_exch_state = round(err_abs_frac_exch_state, 4)) %>%
        rename("Time Point" = time_chosen,
               "Abs Val Exch" = abs_frac_exch_state,
               "Err Abs Val Exch" = err_abs_frac_exch_state)
    }
    
  }
  
}