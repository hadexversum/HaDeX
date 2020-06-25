#' generate_kinetic_data_set
#' 
#' @description Generates the data set of deuterium uptake between selected 
#' time points based on supplied peptide list.
#' 
#' @param dat ...
#' @param peptide_list ...
#' @param protein ...
#' @param time_in ...
#' @param time_out ...
#' @param deut_concentration ...
#' 
#' @details This is a wrapper for \code{\link{calculate_kinetics}}, but for
#' the peptide list instead of one peptide.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_kinetic_data_set

generate_kinetic_data_set <- function(dat,
                                      peptide_list,
                                      protein,
                                      time_in,
                                      time_out,
                                      deut_concentration){
  
  bind_rows(apply(peptide_list, 1, function(peptide){
    calculate_kinetics(dat = dat,
                       protein = protein, 
                       sequence = peptide[1],
                       state = peptide[2],
                       start = as.numeric(peptide[3]),
                       end = as.numeric(peptide[4]),
                       time_in = time_in,
                       time_out = time_out,
                       deut_part = 0.01*as.integer(deut_concentration))
  }))
  
}