#' Calculate deuterium uptake 
#' 
#' @description Calculates deuteration uptake based on supplied parameters.
#' 
#' @importFrom dplyr %>% select mutate group_by coalesce
#' @importFrom data.table fcase fcoalesce
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' @param protein chosen protein. 
#' @param state state included in calculations
#' @param time_0 minimal exchange control
#' @param time_100 maximal exchange control 
#' @param time_t chosen time point 
#' @param deut_part percentage of deuterium the protein was exposed to, 
#' value in range [0, 1]
#' 
#' @details The function \code{calculate_state_uptake} calculates deuterium uptake 
#' (in different forms) for all of the peptides in given protein in given state based
#' on supplied parameters: `time_0`, `time_100` and `time_t`. All four variants 
#' (fractiona) are supplied (mean values and uncertainty). Manual correction of 
#' percentage of deuterium the protein was exposed to during the exchange
#' in theoretical calculations is provided. 
#' 
#' Methods of calculation and uncertainty are profoundly discussed in the vignette.
#' 
#' @return a \code{\link{data.frame}} object
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{create_uptake_dataset}}
#' \code{\link{calculate_confidence_limit_values}} 
#' \code{\link{add_stat_dependency}}
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' calculate_state_uptake(dat)
#' calculate_state_uptake(dat, protein = "db_CD160", state = "CD160",
#'                             time_0 = 0.001, time_t = 5.000, time_100 = 1440.000, deut_part = 1)
#'                             
#'                             
#' @export calculate_state_uptake

calculate_state_uptake <- function(dat,
                                   protein = unique(dat[["Protein"]])[1], 
                                   state = unique(dat[["State"]])[1], 
                                   time_0 = min(dat[dat[["Exposure"]]>0, ][["Exposure"]]),
                                   time_t = unique(dat[["Exposure"]])[3], 
                                   time_100 = max(dat[["Exposure"]]),
                                   deut_part = 0.9){
  
  proton_mass <- 1.00727647
  dat <- data.table(dat[dat[["Protein"]] == protein & dat[["State"]] == state & dat[["Exposure"]] %in% c(time_0, time_t, time_100), ])
  
  uptake_dat <- dat[,  `:=`(exp_mass = Center * z - z * proton_mass,
                            Center = NULL,
                            z = NULL)]
  uptake_dat <- uptake_dat[ , .(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)),
                            by = c("Sequence", "Start", "End", "MHP", "MaxUptake", 
                                   "State", "Exposure", "Protein", "File", "Modification")]
  uptake_dat[, Exposure := fcase(Exposure == time_0, "time_0",
                                 Exposure == time_t, "time_t",
                                 Exposure == time_100, "time_100")]
  uptake_dat <- dcast(uptake_dat, Sequence + Start + End + MHP + MaxUptake + State + Protein + File + Modification + avg_exp_mass ~ Exposure,
                      value.var = "avg_exp_mass")
  uptake_dat <- uptake_dat[ , .(time_0_mean = mean(time_0, na.rm = TRUE),
                                err_time_0_mean = fcoalesce(sd(time_0, na.rm = TRUE)/sqrt(sum(!is.na(time_0))), 0),
                                time_t_mean = mean(time_t, na.rm = TRUE),
                                err_time_t_mean = fcoalesce(sd(time_t, na.rm = TRUE)/sqrt(sum(!is.na(time_t))), 0),
                                time_100_mean = mean(time_100, na.rm = TRUE),
                                err_time_100_mean = fcoalesce(sd(time_100, na.rm = TRUE)/sqrt(sum(!is.na(time_100))), 0)),
                            by = c("Sequence", "Start", "End", "MaxUptake", "MHP", "Protein", "State", "Modification")]
  uptake_dat[, `:=`(frac_deut_uptake = 100*(time_t_mean - time_0_mean)/(time_100_mean - time_0_mean),
                    err_frac_deut_uptake = 100*sqrt((err_time_t_mean*(1/(time_100_mean - time_0_mean)))^2 + (err_time_0_mean*((time_t_mean - time_100_mean )/((time_100_mean - time_0_mean)^2)))^2 + (err_time_100_mean*((time_0_mean - time_t_mean)/((time_100_mean - time_0_mean)^2)))^2),
                    # experimental calculations below - absolute
                    deut_uptake = (time_t_mean - time_0_mean),
                    err_deut_uptake = sqrt(err_time_t_mean^2 + err_time_0_mean^2),
                    # theoretical calculations below - fractional
                    theo_frac_deut_uptake  = 100*(time_t_mean - MHP)/(MaxUptake * proton_mass * deut_part),
                    err_theo_frac_deut_uptake  = 100*abs(err_time_t_mean)*(1/(MaxUptake * proton_mass * deut_part)),
                    # theoretical calculations below - absolute
                    theo_deut_uptake = (time_t_mean - MHP),
                    err_theo_deut_uptake = err_time_t_mean,
                    # helper values
                    Med_Sequence = Start + (End - Start)/2)]
  setorderv(uptake_dat, cols = c("Start", "End"))
  uptake_dat[, `:=`(ID = 1L:nrow(uptake_dat),
                    Exposure = time_t)]
  uptake_dat <- uptake_dat[, .(Protein, Sequence, Exposure, Start, End, State,
                               Modification, frac_deut_uptake, err_frac_deut_uptake,
                               deut_uptake, err_deut_uptake,
                               theo_frac_deut_uptake, err_theo_frac_deut_uptake,
                               theo_deut_uptake, err_theo_deut_uptake,
                               Med_Sequence)]
  
  attr(uptake_dat, "protein") <- protein
  attr(uptake_dat, "state") <- state
  attr(uptake_dat, "time_0") <- time_0
  attr(uptake_dat, "time_t") <- time_t
  attr(uptake_dat, "time_100") <- time_100
  attr(uptake_dat, "deut_part") <- deut_part
  attr(uptake_dat, "has_modification") <- attr(dat, "has_modification")
  
  return(uptake_dat)
  
}
