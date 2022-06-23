#' Calculate measured mass for each replicate of the 
#' experiment
#' 
#' @description Calculate the measured mass from partial results, per each 
#' replicate of the experiment.
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function 
#' 
#' @details Each replicate of the experiment generates measurements of the mass 
#' for obtained charge values for the peptide. This is an effect of the properties
#' of mass spectrometry, that measures the mass to charge ratio (learn more about 
#' Mass Spectrometry in the documentation). The possible charge values depend on
#' the sequence of the peptide.
#' The separate measurement (for each replicate in given state in given time point)
#' can be distinguished by the `File` value.
#' 
#' @return Data frame with aggregated results for each replicate of the experiment.
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{calculate_exp_masses}}
#' \code{\link{calculate_state_uptake}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' calculate_exp_masses_per_replicate(dat)
#' 
#' @export calculate_exp_masses_per_replicate

calculate_exp_masses_per_replicate <- function(dat){
  
  proton_mass <- 1.00727647
  
  dat <- data.table(dat)
  
  exp_dat <- dat[ , `:=`(exp_mass = Center*z - z*proton_mass)]
  exp_dat <- exp_dat[, .(Protein, Start, End, Sequence, MaxUptake, MHP,
                         State, Exposure, File, Modification, Inten, exp_mass)]
  exp_dat <- exp_dat[, .(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)),
                     by = c("Protein", "State", "Sequence", "Start", "End", 
                            "MHP", "MaxUptake", "Exposure", "File", 
                            "Modification")]
  setorderv(exp_dat, cols = c("Start", "End"))
  
  exp_dat <- data.frame(exp_dat)
  
  return(exp_dat)
  
}

#' Calculate measured mass, aggregated from the replicates of the
#' experiment 
#' 
#' @description Calculate the measured mass (with the uncertainty of the measurement)
#' as aggregated data from the replicates of the experiment.
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' 
#' @details Each measurement is repeated at least three times to obtain reliable
#' result and to calculate uncertainty of the measurement. For more information 
#' on how the data is aggregated or how the uncertainty is calculated, see the 
#' documentation. 
#' 
#' @return Data frame with aggregated mass results from replicates of the experiment.
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{calculate_exp_masses_per_replicate}}
#' \code{\link{calculate_state_uptake}} 
#' 
#' @export calculate_exp_masses

calculate_exp_masses <- function(dat){
  
  proton_mass <- 1.00727647
  
  exp_dat <- calculate_exp_masses_per_replicate(dat)
  
  exp_dat <- data.table(exp_dat)
  
  exp_dat <- exp_dat[, .(avg_mass = mean(avg_exp_mass),
                         err_avg_mass = sd(avg_exp_mass)/sqrt(.N)),
                     by = c("Protein", "State", "Sequence", "Start", "End", "MHP", "Exposure", "Modification")]
  
  setorderv(exp_dat, cols = c("Start", "End"))
  
  exp_dat <- data.frame(exp_dat)
  
  return(exp_dat)
  
}


#' Calculate MHP of the peptide
#' 
#' @description Calculate the mass of the singly charged monoisotopic (or not) 
#' molecular ion of for given peptide.
#' 
#' @param Sequence sequence of the peptide (string) or vector of sequences. Each letter 
#' of the sequence of the peptide represents different amino acid (three letter 
#' representation not allowed)
#' @param mono logical value to determine if the mass should be monoisotopic or not. 
#' FALSE by default
#' 
#' @details This function calculates the mass of the singly charged monoisotopic (or not)
#' molecular ion for given peptide. It is the sum of the residue masses plus the masses
#' of the terminationg group (H and OH). The source of the masses can be found here:
#' \url{http://www.matrixscience.com/help/aa_help.html}. Keep in mind that this function
#' returns the value of an unmodified peptide.
#' 
#' @return vector of numeric MHP values of provided Sequences
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{calculate_state_uptake}} 
#' \code{\link{plot_kinetics}}
#' 
#' @export calculate_MHP

calculate_MHP <- function(Sequence, mono = FALSE){
  
  amino_mass_mono <- c("A" = 71.037114, "R" = 156.101111, "N" = 114.042927, "D" = 115.026943, "C" = 103.009185, "E" = 129.042593, "Q" = 128.058578, "G" = 57.021464, "H" = 137.058912, "I" = 113.084064, "L" = 113.084064, "K" = 128.094963, "M" = 131.040485, "F" = 147.068414, "P" = 97.052764, "V" = 99.068414, "S" = 87.032028, "T" = 101.047679, "U" = 150.95363, "W" = 186.079313, "Y" = 163.06332)
  
  amino_mass_nomono <- c("A" = 71.0779, "R" = 156.1857, "N" = 114.1026, "D" = 115.0874, "C" = 103.1429, "E" = 129.114, "Q" = 128.1292, "G" = 57.0513, "H" = 137.1393, "I" = 113.1576, "L" = 113.1576, "K" = 128.1723, "M" = 131.1961, "F" = 147.1739, "P" = 97.1152, "S" = 87.0773, "T" = 101.1039, "U" = 150.0379, "W" = 186.2099, "Y" = 163.1733, "V" = 99.1311)
  
  terminating_group_mass_mono <- 18.01056
  terminating_group_mass_nomono <- 18.01524
  
  if(mono) {
    unlist(lapply(Sequence, function(s){
      sum(amino_mass_mono[strsplit(s, "")[[1]]]) + terminating_group_mass_mono
    }))
  } else {
    unlist(lapply(Sequence, function(s){
      sum(amino_mass_nomono[strsplit(s, "")[[1]]]) + terminating_group_mass_nomono
    }))
  }
  
}