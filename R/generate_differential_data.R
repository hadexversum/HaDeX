#' generate_differential_data
#' 
#' @description Generate the data frame with differential data from two
#' provided states - experimental/relative calculations with the
#' uncertainty
#' 
#' @param dat ...
#' @param states vector of two states to calculate difference between them, 
#' the order is important
#' @param protein ...
#' @param time_in ...
#' @param time_chosen ...
#' @param time_out ...
#' @param deut_part ...
#' 
#' @details The names of the parameters will be changed to more descriptive 
#' ones after the glossary project.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_differential_data

generate_differential_data <- function(dat,
                                       states,
                                       protein,
                                       time_in,
                                       time_chosen,
                                       time_out,
                                       deut_part){
  
  bind_rows(lapply(states, function(i) calculate_state_deuteration(dat, 
                                                                   protein = protein, 
                                                                   state = i, 
                                                                   time_in = time_in,
                                                                   time_chosen = time_chosen, 
                                                                   time_out = time_out,
                                                                   deut_part = deut_part))) %>%
    droplevels() %>% 
    mutate(State = factor(State, levels = states, labels = c("1", "2"))) %>%
    gather(variable, value, -c(Protein:End, State, Med_Sequence)) %>%
    unite(tmp, variable, State) %>%
    spread(tmp, value)  %>%
    mutate(diff_frac_exch = frac_exch_state_1 - frac_exch_state_2,
           err_frac_exch = sqrt(err_frac_exch_state_1^2 + err_frac_exch_state_2^2),
           abs_diff_frac_exch = abs_frac_exch_state_1 - abs_frac_exch_state_2,
           err_abs_diff_frac_exch = sqrt(err_abs_frac_exch_state_1^2 + err_abs_frac_exch_state_2^2),
           diff_theo_frac_exch = avg_theo_in_time_1 - avg_theo_in_time_2, 
           err_diff_theo_frac_exch = sqrt(err_avg_theo_in_time_1^2 + err_avg_theo_in_time_2^2),
           abs_diff_theo_frac_exch = abs_avg_theo_in_time_1 - abs_avg_theo_in_time_2,
           err_abs_diff_theo_frac_exch = sqrt(err_abs_avg_theo_in_time_1^2 + err_abs_avg_theo_in_time_2^2)) %>%
    select(Protein, Start, End, Med_Sequence, everything(), -contains("1"), -contains("2"))
}