#' calculate_state_deuteration
#' 
#' @param dat dat
#' @param protein protein
#' @param state state
#' @param time_in time in
#' @param time_chosen time chosen
#' @param time_out time out
#' 
#' @return data.frame 
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' calculate_state_deuteration(dat, protein = "db_CD160", state = "CD160",
#'                             time_in = 0, time_chosen = 5.000, time_out = 1440.000)
#' 
#' @export calculate_state_deuteration

calculate_state_deuteration <- function(dat,
                                        protein, 
                                        state, 
                                        time_in,
                                        time_chosen, 
                                        time_out
                                        ){
  proton_mass <- 1.00727647
  dat <- dat[dat[["Protein"]] == protein & dat[["State"]] == state & dat[["Exposure"]] %in% c(time_in, time_chosen, time_out), ]
  
  dat %>%
    mutate(exp_mass = Center*z - z*proton_mass) %>%
    select(-Center, -z, -Modification, -Fragment) %>%
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, Protein, File) %>%
    summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
    ungroup(.) %>%
    mutate(Exposure = case_when(Exposure == time_in ~ "time_in",
                                Exposure == time_chosen ~ "time_chosen",
                                Exposure == time_out ~ "time_out")) %>%
    spread(key = Exposure, value = avg_exp_mass) %>%
    group_by(Sequence, Start, End, MaxUptake, MHP, Protein, State) %>%
    summarize(time_in_mean = mean(time_in, na.rm = TRUE),
              err_time_in_mean = coalesce(sd(time_in, na.rm = TRUE)/sqrt(length(time_in)), 0),
              time_chosen_mean = mean(time_chosen, na.rm = TRUE),
              err_time_chosen_mean = coalesce(sd(time_chosen, na.rm = TRUE)/sqrt(length(time_chosen)), 0),
              time_out_mean = mean(time_out, na.rm = TRUE),
              err_time_out_mean = coalesce(sd(time_out, na.rm = TRUE)/sqrt(length(time_out)), 0)) %>%
    mutate(# experimental calculations below - relative
      frac_exch_state = (time_chosen_mean - time_in_mean)/(time_out_mean - time_in_mean),
      err_frac_exch_state = sqrt((err_time_chosen_mean*(1/(time_out_mean - time_in_mean)))^2 + (err_time_in_mean*((time_chosen_mean - time_out_mean )/((time_out_mean - time_in_mean)^2)))^2 + (err_time_out_mean*((time_in_mean - time_chosen_mean)/((time_out_mean - time_in_mean)^2)))^2),
      # experimental calculations below - absolute
      abs_frac_exch_state = time_chosen_mean - time_in_mean,
      err_abs_frac_exch_state = sqrt(err_time_chosen_mean^2 + err_time_in_mean^2),
      # theoretical calculations below - relative
      avg_theo_in_time = (time_chosen_mean - MHP)/(MaxUptake * proton_mass),
      err_avg_theo_in_time = abs(err_time_chosen_mean)*(1/(MaxUptake * proton_mass)),
      # theoeretical calculations below - absolute
      abs_avg_theo_in_time = time_chosen_mean - MHP,
      err_abs_avg_theo_in_time = err_time_chosen_mean,
      # helper values
      Med_Sequence = Start + (End - Start)/2) %>%
    ungroup(.) %>%
    arrange(Start, End) %>%
    select(Protein, Sequence, Start, End, State, frac_exch_state, err_frac_exch_state, abs_frac_exch_state, err_abs_frac_exch_state, avg_theo_in_time, err_avg_theo_in_time,abs_avg_theo_in_time, err_abs_avg_theo_in_time, Med_Sequence)

}