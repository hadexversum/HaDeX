#' Create kinetics dataset for a list of peptides and their states
#' 
#' @description Generates the data set of deuterium uptake between selected 
#' time points based on supplied peptide list.
#' 
#' @param dat dat data imported by the \code{\link{read_hdx}} function.
#' @param peptide_list list of peptides for the calculation.
#' @param protein chosen protein. 
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement.
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details This is a wrapper for \code{\link{calculate_kinetics}}, but for
#' the peptide list instead of one peptide. 
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
#' peptide_list <- data.frame(Sequence = c("INITSSASQEGTRLN", "INITSSASQEGTRLN"), 
#'                            state = c("CD160", "CD160_HVEM"), 
#'                            start = c(1, 1), end = c(15, 15))
#' create_kinetic_dataset(dat, peptide_list)
#' 
#' peptide_list2 <- data.frame(Sequence = c("INITSSASQEGTRLN", "LICTVW"), 
#'                             state = c("CD160", "CD160"), 
#'                             start = c(1, 16), end = c(15, 21))
#' create_kinetic_dataset(dat, peptide_list2)
#' 
#' @export create_kinetic_dataset

create_kinetic_dataset <- function(dat,
                                   peptide_list,
                                   protein = dat[["Protein"]][1],
                                   time_0 = min(dat[["Exposure"]]),
                                   time_100 = max(dat[["Exposure"]]),
                                   deut_part = 0.9){
  
  dat <- as.data.table(dat)
  
  kin_dat <- rbindlist(apply(peptide_list, 1, function(peptide){
    calculate_kinetics(dat = dat,
                       protein = protein, 
                       sequence = peptide[1],
                       state = peptide[2],
                       start = as.numeric(peptide[3]),
                       end = as.numeric(peptide[4]),
                       time_0 = time_0,
                       time_100 = time_100,
                       deut_part = deut_part)
  }))
  
  attr(kin_dat, "protein") <- protein
  attr(kin_dat, "peptide_list") <- peptide_list
  attr(kin_dat, "time_0") <- time_0
  attr(kin_dat, "time_100") <- time_100
  attr(kin_dat, "deut_part") <- deut_part
  attr(kin_dat, "has_modification") <- attr(dat, "has_modification")
  
  kin_dat <- as.data.frame(kin_dat)
  
  return(kin_dat)
  
}