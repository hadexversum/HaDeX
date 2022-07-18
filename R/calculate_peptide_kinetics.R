#' Calculate kinetics dataset 
#' 
#' @description Calculate kinetics of the hydrogen-deuteration exchange 
#' for given peptide in multiple biological states.
#' 
#' @param dat dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states vector of states (for chosen protein), for which the 
#' calculations are done. 
#' @param sequence sequence of chosen peptide.
#' @param start start position of chosen peptide.
#' @param end end position of chosen peptide.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement.
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details Function \code{\link{calculate_peptide_kinetics}} calculates
#' kinetic data for chosen peptide in chosen biological states.
#' It is a wrapper for \code{\link{calculate_kinetics}} but for mutltiple
#' states.
#' The output of this function can be visualized using \code{\link{plot_uptake_curve}}.
#' IMPORTANT! The kinetic data is often described as deuterium uptake curve data. 
#' We use this terms interchangeable. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{calculate_kinetics}}
#' \code{\link{calculate_state_uptake}}
#' \code{\link{plot_uptake_curve}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' calculate_peptide_kinetics(dat)
#' 
#' @export calculate_peptide_kinetics

calculate_peptide_kinetics <- function(dat,
                                       protein = dat[["Protein"]][1],
                                       sequence = dat[["Sequence"]][1],
                                       states = unique(dat[["State"]]), 
                                       start = dat[["Start"]][1], 
                                       end = dat[["End"]][1], 
                                       time_0 = min(dat[["Exposure"]]),
                                       time_100 = max(dat[["Exposure"]]),
                                       deut_part = 0.9){
  
  dat <- data.table(dat)
  
  kin_dat <- rbindlist(lapply(states, function(state){
    
    calculate_kinetics(dat = dat,
                       protein = protein, 
                       sequence = sequence, 
                       state = state,
                       start = start,
                       end = end, 
                       time_0 = time_0,
                       time_100 = time_100,
                       deut_part = deut_part)
    
  }))
  
  kin_dat <- data.frame(kin_dat)
  
  attr(kin_dat, "protein") <- protein
  attr(kin_dat, "sequence") <- sequence
  attr(kin_dat, "states") <- states
  attr(kin_dat, "start") <- start
  attr(kin_dat, "end") <- end
  attr(kin_dat, "time_0") <- time_0
  attr(kin_dat, "time_100") <- time_100
  attr(kin_dat, "deut_part") <- deut_part
  attr(kin_dat, "has_modification") <- attr(dat, "has_modification")
  
  return(kin_dat)
  
}

