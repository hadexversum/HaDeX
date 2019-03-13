#' prepare_dataset
#' 
#' Calculates values for visualization  from input data file - both experimental and theoretical. All parameters are needed.
#' 
#' @importFrom dplyr %>% mutate select group_by summarize ungroup sym coalesce arrange
#' @importFrom tidyr unite spread
#' @importFrom stats sd weighted.mean
#' 
#' @param dat data frame with data from Dynamix file
#' @param in_state_first string in form "state_time" for first state in in time
#' @param chosen_state_first string in form "state_time" for chosen state in in time
#' @param out_state_first string in form "state_time" for first state in out time
#' @param in_state_second string in form "state_time" for second state in in time
#' @param chosen_state_second string in form "state_time" for second state in chosen time
#' @param out_state_second string in form "state_time" for second state in out time
#' 
#' @return data frame with caluclated values
#' 
#' @export prepare_dataset

prepare_dataset <- function(dat,
                            in_state_first,
                            chosen_state_first,
                            out_state_first,
                            in_state_second,
                            chosen_state_second,
                            out_state_second){
  proton_mass <- 1.00727647
  
  dat %>%
    mutate(exp_mass = Center*z - z*proton_mass) %>%
    select(-Center, -z, -Protein) %>%
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, File) %>%
    summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
    ungroup() %>%
    unite(State_Exposure, State, Exposure) %>%
    spread(key = State_Exposure, value = avg_exp_mass) %>%
    group_by(Sequence, Start, End, MaxUptake, MHP) %>%
    summarize(in_time_mean_1 = mean(!!sym(in_state_first), na.rm = TRUE),
              err_in_time_mean_1 = coalesce(sd(!!sym(in_state_first), na.rm = TRUE)/sqrt(length(in_state_first)), 0),
              chosen_time_mean_1 = mean(!!sym(chosen_state_first), na.rm = TRUE),
              err_chosen_time_mean_1 = sd(!!sym(chosen_state_first), na.rm = TRUE)/sqrt(length(chosen_state_first)),
              out_time_mean_1 = mean(!!sym(out_state_first), na.rm = TRUE),
              err_out_time_mean_1 = sd(!!sym(out_state_first), na.rm = TRUE)/sqrt(length(out_state_first)),
              in_time_mean_2 = mean(!!sym(in_state_second), na.rm = TRUE),
              err_in_time_mean_2 = coalesce(sd(!!sym(in_state_second), na.rm = TRUE)/sqrt(length(in_state_second)), 0),
              chosen_time_mean_2 = mean(!!sym(chosen_state_second), na.rm = TRUE),
              err_chosen_time_mean_2 = sd(!!sym(chosen_state_second), na.rm = TRUE)/sqrt(length(chosen_state_second)),
              out_time_mean_2 = mean(!!sym(out_state_second), na.rm = TRUE),
              err_out_time_mean_2 = sd(!!sym(out_state_second), na.rm = TRUE)/sqrt(length(out_state_second))) %>%
    mutate(frac_exch_state_1 = (chosen_time_mean_1 - in_time_mean_1)/(out_time_mean_1 - in_time_mean_1),
           # err_frac_exch_state_1 = sqrt(err_chosen_time_mean_first^2 + 2*err_in_time_mean_first^2 + err_in_time_mean_first^2), 
           err_frac_exch_state_1 = sqrt((err_chosen_time_mean_1*(1/(out_time_mean_1 - in_time_mean_1)))^2 + (err_in_time_mean_1*((chosen_time_mean_1 - out_time_mean_1 )/((out_time_mean_1 - in_time_mean_1)^2)))^2 + (err_out_time_mean_1*((in_time_mean_1-chosen_time_mean_1)/((out_time_mean_1 - in_time_mean_1)^2)))^2),
           frac_exch_state_2 = (chosen_time_mean_2 - in_time_mean_2)/(out_time_mean_2 - in_time_mean_2),
           # err_frac_exch_state_2 = sqrt(err_chosen_time_mean_second^2 + 2*err_in_time_mean_second^2 + err_in_time_mean_second^2),
           err_frac_exch_state_2 = sqrt((err_chosen_time_mean_2*(1/(out_time_mean_2 - in_time_mean_2)))^2 + (err_in_time_mean_2*((chosen_time_mean_2 - out_time_mean_2 )/((out_time_mean_2 - in_time_mean_2)^2)))^2 + (err_out_time_mean_2*((in_time_mean_2-chosen_time_mean_2)/((out_time_mean_2 - in_time_mean_2)^2)))^2),
           avg_theo_in_time_1 = (chosen_time_mean_1 - MHP)/(MaxUptake * proton_mass),
           err_avg_theo_in_time_1 = abs(err_chosen_time_mean_1)*(1/(MaxUptake * proton_mass)),
           avg_theo_in_time_2 = (chosen_time_mean_2 - MHP)/(MaxUptake * proton_mass),
           err_avg_theo_in_time_2 = abs(err_chosen_time_mean_2*(1/(MaxUptake * proton_mass))),
           diff_frac_exch = frac_exch_state_1 - frac_exch_state_2,
           err_frac_exch = sqrt(err_frac_exch_state_1^2 + err_frac_exch_state_2^2),
           diff_theo_frac_exch = avg_theo_in_time_1 - avg_theo_in_time_2, 
           err_diff_theo_frac_exch = sqrt(err_avg_theo_in_time_1^2 + err_avg_theo_in_time_2^2),
           Med_Sequence = Start + (End - Start)/2) %>%
    ungroup(.) %>%
    select(Sequence, Start, End, Med_Sequence, 
           frac_exch_state_1, err_frac_exch_state_1, frac_exch_state_2, err_frac_exch_state_2, diff_frac_exch, err_frac_exch, 
           avg_theo_in_time_1, err_avg_theo_in_time_1, avg_theo_in_time_2, err_avg_theo_in_time_2, diff_theo_frac_exch, err_diff_theo_frac_exch) %>%
    arrange(Start, End)
  
}
