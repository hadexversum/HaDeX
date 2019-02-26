#' prepare_dataset
#' 
#' Calculates values for visualization  from input data file - both experimental and theoretical. All parameters are needed.
#' 
#' @importFrom dplyr %>% mutate select group_by summarize ungroup sym coalesce arrange
#' @importFrom tidyr unite spread
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
    mutate(exp_mass = Center*z - z*proton_mass,
           Exposure = round(Exposure, 3)) %>%
    select(-Center, -z, -Protein) %>%
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, File) %>%
    summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
    ungroup() %>%
    unite(State_Exposure, State, Exposure) %>%
    spread(key = State_Exposure, value = avg_exp_mass) %>%
    mutate(theo_in_time_first = (!!sym(chosen_state_first) - MHP)/ (MaxUptake * proton_mass),
           theo_in_time_second = (!!sym(chosen_state_second) - MHP)/(MaxUptake * proton_mass)) %>%
    group_by(Sequence, Start, End) %>%
    summarize(in_time_mean_first = mean(!!sym(in_state_first), na.rm = TRUE),
              err_in_time_mean_first = coalesce(sd(!!sym(in_state_first), na.rm = TRUE), 0),
              chosen_time_mean_first = mean(!!sym(chosen_state_first), na.rm = TRUE),
              err_chosen_time_mean_first = sd(!!sym(chosen_state_first), na.rm = TRUE),
              out_time_mean_first = mean(!!sym(out_state_first), na.rm = TRUE),
              err_out_time_mean_first = sd(!!sym(out_state_first), na.rm = TRUE),
              in_time_mean_second = mean(!!sym(in_state_second), na.rm = TRUE),
              err_in_time_mean_second = coalesce(sd(!!sym(in_state_second), na.rm = TRUE), 0),
              chosen_time_mean_second = mean(!!sym(chosen_state_second), na.rm = TRUE),
              err_chosen_time_mean_second = sd(!!sym(chosen_state_second), na.rm = TRUE),
              out_time_mean_second = mean(!!sym(out_state_second), na.rm = TRUE),
              err_out_time_mean_second = sd(!!sym(out_state_second), na.rm = TRUE),
              avg_theo_in_time_1 = mean(theo_in_time_first, na.rm = TRUE),
              err_avg_theo_in_time_1 = sd(theo_in_time_first, na.rm = TRUE),
              avg_theo_in_time_2 = mean(theo_in_time_second, na.rm = TRUE),
              err_avg_theo_in_time_2 = sd(theo_in_time_second, na.rm = TRUE))  %>%
    mutate(frac_exch_state_1 = (chosen_time_mean_first - in_time_mean_first)/(out_time_mean_first - in_time_mean_first),
           err_frac_exch_state_1 = sqrt(err_chosen_time_mean_first^2 + 2*err_in_time_mean_first^2 + err_in_time_mean_first^2), 
           frac_exch_state_2 = (chosen_time_mean_second - in_time_mean_second)/(out_time_mean_second - in_time_mean_second),
           err_frac_exch_state_2 = sqrt(err_chosen_time_mean_second^2 + 2*err_in_time_mean_second^2 + err_in_time_mean_second^2),
           diff_frac_exch = frac_exch_state_1 - frac_exch_state_2,
           err_frac_exch = sqrt(err_frac_exch_state_1^2 + err_frac_exch_state_2^2),
           diff_theo_frac_exch = avg_theo_in_time_1 - avg_theo_in_time_2, 
           err_diff_theo_frac_exch = sqrt(err_avg_theo_in_time_1^2 + err_avg_theo_in_time_2^2),
           Med_Sequence = Start + (End - Start)/2) %>%
    select(Sequence, Start, End, Med_Sequence, 
           frac_exch_state_1, err_frac_exch_state_1, frac_exch_state_2, err_frac_exch_state_2, diff_frac_exch, err_frac_exch, 
           avg_theo_in_time_1, err_avg_theo_in_time_1, avg_theo_in_time_2, err_avg_theo_in_time_2, diff_theo_frac_exch, err_diff_theo_frac_exch) %>%
    arrange(Start, End)
  
}