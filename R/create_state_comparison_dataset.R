#' Creates comparison uptake dataset
#' 
#' @description Calculates deuterium uptake values for selected 
#' biological states in selected time point of measurements.
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states vector of states (for chosen protein), for which the 
#' calculations are done. 
#' @param time_0 minimal exchange control time point of measurement [min].
#' @param time_t time point of the measurement for which the calculations
#' are done [min]. 
#' @param time_100 maximal exchange control time point of measurement [min].
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details Function \code{\link{create_state_comparison_dataset}} is a 
#' wrapper for \code{\link{calculate_state_uptake}} function, calls 
#' this function for all (default) or chosen states in states vector.
#' 
#' @return a \code{\link{data.frame}} object. 
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' comparison_dat <- create_state_comparison_dataset(dat)
#' head(comparison_dat)
#'
#' @export create_state_comparison_dataset

create_state_comparison_dataset <- function(dat,
                                            protein = unique(dat[["Protein"]])[1],
                                            states = unique(dat[["State"]]),
                                            time_0 = min(dat[["Exposure"]]),
                                            time_t = unique(dat[["Exposure"]])[3], 
                                            time_100 = max(dat[["Exposure"]]),
                                            deut_part = 0.9){
  
  dat <- data.table(dat)
  
  comparison_dat <- rbindlist(lapply(states, function(state){
    
    calculate_state_uptake(dat,
                           protein = protein,
                           state = state,
                           time_0 = time_0,
                           time_t = time_t,
                           time_100 = time_100,
                           deut_part = deut_part)
    
  }))
  
  comparison_dat <- data.frame(comparison_dat)
  
  attr(comparison_dat, "protein") <- protein
  attr(comparison_dat, "states") <- states
  attr(comparison_dat, "time_0") <- time_0
  attr(comparison_dat, "time_100") <- time_100
  attr(comparison_dat, "deut_part") <- deut_part
  attr(comparison_dat, "has_modification") <- attr(dat, "has_modification")
  
  return(comparison_dat)
  
}