#' Create uptake dataset for multiple states
#'
#' @description Calculates deuterium uptake values for selected 
#' biological states in multiple time points of measurements.
#' 
#' @importFrom dplyr arrange filter summarise 
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states list of biological states for chosen protein.
#' @param time_0 minimal exchange control time point of measurement [min].
#' @param time_100 maximal exchange control time point of measurement [min]. 
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details Function \code{\link{create_uptake_dataset}} generates 
#' a dataset with deuterium uptake values in different forms. For each
#' peptide in chosen protein in chosen states for time points of measurement
#' between minimal and maximal control time points of measurement deuterium 
#' uptake, fractional deuterium uptake with respect to controls or theoretical
#' tabular values are calculated, with combined and propagated uncertainty. 
#' Each peptide has an ID, based on its start position. 
#' This function is a wrapper for \code{\link{create_state_uptake_dataset}}
#' but for multiple states. 
#' The output of this function can be presented in a form of 
#' comparison plot.
#'
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}
#' \code{\link{create_state_uptake_dataset}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' uptake_dat <- create_uptake_dataset(dat, states = c("CD160", "CD160_HVEM"))
#' head(uptake_dat)
#' 
#' @export create_uptake_dataset

create_uptake_dataset <- function(dat,
                                  protein = unique(dat[["Protein"]])[1],
                                  states = unique(dat[["State"]]),
                                  time_0 = min(dat[["Exposure"]]),
                                  time_100 = max(dat[["Exposure"]]),
                                  deut_part = 0.9){
  
  dat <- as.data.table(dat)
  
  times <- unique(dat[["Exposure"]])
  times <- times[times > time_0]
  
  uptake_dat <- rbindlist(lapply(states, function(state){
    
    rbindlist(lapply(times, function(time){
      
      calculate_state_uptake(dat, protein = protein, 
                             state = state,
                             time_t = time, 
                             time_0 = time_0, time_100 = time_100,
                             deut_part = deut_part)
      
    }))
    
  }))
  
  attr(uptake_dat, "protein") <- protein
  attr(uptake_dat, "state") <- NULL
  attr(uptake_dat, "time_t") <- NULL
  attr(uptake_dat, "states") <- states
  attr(uptake_dat, "time_0") <- time_0
  attr(uptake_dat, "time_100") <- time_100
  attr(uptake_dat, "deut_part") <- deut_part
  attr(uptake_dat, "has_modification") <- attr(dat, "has_modification")
  
  uptake_dat <- as.data.frame(uptake_dat)
  
  return(uptake_dat)
  
}
