#' Calculate Area Under the Curve
#' 
#' @description Calculates area under the deuterium uptake curve
#' 
#' @param uptake_dat data with deuterium uptake values, 
#' calculated e.q. by \code{\link{create_uptake_dataset}}
#' @param protein ...
#' @param state ...
#' @param preserve_values ...
#' @param time_100 ...
#' 
#' @details The AUC is calculated on the data normalized to unit square by 
#' division by maximum values of exposure time and deuterium uptake, 
#' respectively.
#' 
#' @return a \code{\link{data.frame}} object
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{create_uptake_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' uptake_dat <- create_uptake_dataset(dat)
#' calculate_auc(uptake_dat)
#' 
#' @export calculate_auc

calculate_auc <- function(uptake_dat,
                          protein = uptake_dat[["Protein"]][1],
                          state = uptake_dat[["State"]][1],
                          preserve_values = F, 
                          time_100 = NULL) {
  
  if(is.null(time_100)) { time_100 <- attr(uptake_dat, "time_100") }
  
  uptake_dat <- as.data.table(uptake_dat)
  
  uptake_dat <- uptake_dat[Protein == protein & State == state]
  
  uptake_dat[, `:=`(deut_uptake_sig = round(deut_uptake, 0),
                    exposure_norm = Exposure/max(Exposure)),
             by = c("Protein", "State", "Sequence", "Start", "End")]
  uptake_dat[, `:=`(deuterium_uptake_norm = deut_uptake_sig/max(deut_uptake_sig[Exposure == time_100])),
             by = c("Protein", "State", "Sequence", "Start", "End")]
  
  uptake_dat[, auc := {
    h <- exposure_norm[-1] - exposure_norm[-length(exposure_norm)]
    ab <- deuterium_uptake_norm[-length(deuterium_uptake_norm)] + deuterium_uptake_norm[-1]
    auc <- sum(ab * h * 0.5)
    auc
  },
  by = c("Protein", "State", "Sequence", "Start", "End")]
  
  uptake_dat[, auc := ifelse(is.na(auc), 0, auc)]
  
  if(!preserve_values) {
    
    uptake_dat <- unique(uptake_dat[ , .(Protein, Sequence, Start, End, State, Modification, auc)])
  }
  
  data.table(uptake_dat)
  
}
