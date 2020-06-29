#' generate_comparison_data
#' 
#' @description Generates data visible on the comparison plot.
#' 
#' @param dat custom data format, produced by \code{\link{calculate_state_deuteration}}
#' @param theoretical ...
#' @param relative ...
#' 
#' @details This data is available in the GUI. The names of the parameters
#' will be changed later after the glossary project.
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
                                     relative,
                                     protein){
  if (theoretical){
    
    if (relative){
      # theoretical & relative
      dat %>%
        select(Protein, Sequence, State, Start, End, avg_theo_in_time, err_avg_theo_in_time) %>%
        mutate(avg_theo_in_time = round(avg_theo_in_time, 4),
               err_avg_theo_in_time = round(err_avg_theo_in_time, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Frac Exch" = avg_theo_in_time, 
               "Err Theo Frac Exch" = err_avg_theo_in_time)
      
    } else {
      # theoretical & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, abs_avg_theo_in_time, err_abs_avg_theo_in_time) %>%
        mutate(abs_avg_theo_in_time = round(abs_avg_theo_in_time, 4),
               err_abs_avg_theo_in_time = round(err_abs_avg_theo_in_time, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Abs Val Exch" = abs_avg_theo_in_time,
               "Err Theo Abs Val Exch" = err_abs_avg_theo_in_time)
    }
    
  } else {
    
    if (relative){
      # experimental & relative
      dat %>%
        select(Protein, Sequence, State, Start, End, frac_exch_state, err_frac_exch_state) %>%
        mutate(frac_exch_state = round(frac_exch_state, 4),
               err_frac_exch_state = round(err_frac_exch_state, 4)) %>%
        arrange(Start, End) %>%
        rename("Frac Exch" = frac_exch_state,
               "Err Frac Exch" = err_frac_exch_state)
      
    } else {
      # experimental & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, abs_frac_exch_state, err_abs_frac_exch_state) %>%
        mutate(abs_frac_exch_state = round(abs_frac_exch_state, 4),
               err_abs_frac_exch_state = round(abs_frac_exch_state, 4)) %>%
        arrange(Start, End) %>%
        rename("Abs Val Exch" = abs_frac_exch_state,
               "Err Abs Val Exch" = err_abs_frac_exch_state)
      
    }
    
  }
  
}