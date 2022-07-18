#' Calculate Area Under the Curve for deuterium uptake curves
#' 
#' @description This function calculates area under the deuterium uptake curve. 
#' 
#' @param uptake_dat 
#' 
#' @details The AUC is calculated on the data standarized to unit square by 
#' division by maximum values of exposure time and deuterium uptake, 
#' respectively.
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' uptake_dat <- create_uptake_dataset(dat)
#' calculate_auc(uptake_dat)
#' 
#' @export calculate_auc
#' 

calculate_auc <- function(uptake_dat,
                          protein = uptake_dat[["Protein"]][1],
                          state = uptake_dat[["State"]][1],
                          sequence = uptake_dat[["Sequence"]][1]) {
  
  uptake_dat <- as.data.table(uptake_dat)
  
  uptake_dat <- uptake_dat[Protein == protein & State == state & Sequence == sequence]
  
  uptake_dat[, `:=`(deut_uptake_sig = round(deut_uptake, 0),
                    exposure_norm = Exposure/max(Exposure)),
             by = c("Protein", "State", "Sequence", "Start", "End")]
  uptake_dat[, `:=`(deuterium_uptake_norm = deut_uptake_sig/max(deut_uptake_sig[Exposure == 1440])),
             by = c("Protein", "State", "Sequence", "Start", "End")]
  
  uptake_dat[, auc := {
    h <- exposure_norm[-1] - exposure_norm[-length(exposure_norm)]
    ab <- deuterium_uptake_norm[-length(deuterium_uptake_norm)] + deuterium_uptake_norm[-1]
    auc <- sum(ab * h * 0.5)
    auc
  },
  by = c("Protein", "State", "Sequence", "Start", "End")]
  
  uptake_dat[, auc := ifelse(is.na(auc), 0, auc)]
  
  uptake_dat
  
}
