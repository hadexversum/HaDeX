#' Calculate deuteration 
#' 
#' @description Calculates deuteration uptake based on supplied parameters.
#' 
#' @importFrom dplyr %>% select mutate group_by coalesce
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' @param protein protein included in calculations
#' @param state state included in calculations
#' @param time_0 minimal exchange control
#' @param time_100 maximal exchange control 
#' @param time_t chosen time point 
#' @param deut_part percentage of deuterium the protein was exposed to, value in range [0, 1]
#' 
#' @details The function \code{calculate_state_deuteration} calculates deuterium uptake for peptides in given protein in given state based
#' on supplied parameters: `time_0`, `time_100` and `time_t`. All four variants (combinations of theoretical & relative) are 
#' supplied (mean values and uncertainty). Manual correction of percentage of deuterium the protein was exposed to during the exchange
#' in theoretical calculations is provided. 
#'
#' Methods of calculation and uncertainty are profoundly discussed in the vignette.
#' 
#' @return a \code{\link{data.frame}} object
#' 
#' @seealso \code{\link{read_hdx}} \code{\link{calculate_confidence_limit_values}} \code{\link{add_stat_dependency}}
#' 
#' @examples
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' # calculate deuterium uptake values for state "CD160"
#' calculate_state_deuteration(dat, protein = "db_CD160", state = "CD160",
#'                             time_0 = 0.001, time_t = 5.000, time_100 = 1440.000)
#'                             
#' # calculate deuterium uptake values for state "CD160_HVEM"
#' calculate_state_deuteration(dat, protein = "db_CD160", state = "CD160_HVEM",
#'                             time_0 = 0.001, time_t = 5.000, time_100 = 1440.000)
#'                             
#' @export calculate_state_deuteration

calculate_state_deuteration <- function(dat,
                                        protein, 
                                        state, 
                                        time_0,
                                        time_t, 
                                        time_100,
                                        deut_part = 1){
  proton_mass <- 1.00727647
  dat <- dat[dat[["Protein"]] == protein & dat[["State"]] == state & dat[["Exposure"]] %in% c(time_0, time_t, time_100), ]
  
  dat %>%
    mutate(exp_mass = Center*z - z*proton_mass) %>%
    select(-Center, -z, -Modification, -Fragment) %>%
    group_by(Sequence, Start, End, MHP, MaxUptake, State, Exposure, Protein, File) %>%
    summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
    ungroup(.) %>%
    mutate(Exposure = case_when(Exposure == time_0 ~ "time_0",
                                Exposure == time_t ~ "time_t",
                                Exposure == time_100 ~ "time_100")) %>%
    spread(key = Exposure, value = avg_exp_mass) %>%
    group_by(Sequence, Start, End, MaxUptake, MHP, Protein, State) %>%
    summarize(time_0_mean = mean(time_0, na.rm = TRUE),
              err_time_0_mean = coalesce(sd(time_0, na.rm = TRUE)/sqrt(sum(!is.na(time_0))), 0),
              time_t_mean = mean(time_t, na.rm = TRUE),
              err_time_t_mean = coalesce(sd(time_t, na.rm = TRUE)/sqrt(sum(!is.na(time_t))), 0),
              time_100_mean = mean(time_100, na.rm = TRUE),
              err_time_100_mean = coalesce(sd(time_100, na.rm = TRUE)/sqrt(sum(!is.na(time_100))), 0)) %>%
    mutate(# experimental calculations below - fractional
      frac_deut_uptake = 100*(time_t_mean - time_0_mean)/(time_100_mean - time_0_mean),
      err_frac_deut_uptake = 100*sqrt((err_time_t_mean*(1/(time_100_mean - time_0_mean)))^2 + (err_time_0_mean*((time_t_mean - time_100_mean )/((time_100_mean - time_0_mean)^2)))^2 + (err_time_100_mean*((time_0_mean - time_t_mean)/((time_100_mean - time_0_mean)^2)))^2),
      # experimental calculations below - absolute
      deut_uptake = (time_t_mean - time_0_mean),
      err_deut_uptake = sqrt(err_time_t_mean^2 + err_time_0_mean^2),
      # theoretical calculations below - fractional
      theo_frac_deut_uptake  = 100*(time_t_mean - MHP)/(MaxUptake * proton_mass * deut_part),
      err_theo_frac_deut_uptake  = 100*abs(err_time_t_mean)*(1/(MaxUptake * proton_mass * deut_part)),
      # theoeretical calculations below - absolute
      theo_deut_uptake = (time_t_mean - MHP),
      err_theo_deut_uptake = err_time_t_mean,
      # helper values
      Med_Sequence = Start + (End - Start)/2) %>%
    ungroup(.) %>%
    arrange(Start, End) %>%
    select(Protein, Sequence, Start, End, State, 
           frac_deut_uptake, err_frac_deut_uptake, 
           deut_uptake, err_deut_uptake, 
           theo_frac_deut_uptake, err_theo_frac_deut_uptake,
           theo_deut_uptake, err_theo_deut_uptake, 
           Med_Sequence)

}
