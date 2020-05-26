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
#' \url{http://www.matrixscience.com/help/aa_help.html}.
#' 
#' @return vector of numeric MHP values of provided Sequences
#' 
#' @seealso 
#' \code{\link{read_hdx}} \code{\link{calculate_state_deuteration}} 
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