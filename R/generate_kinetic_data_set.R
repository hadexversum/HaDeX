#' generate_kinetic_data_set
#' 
#' @description Generates the data set of deuterium uptake between selected 
#' time points based on supplied peptide list.
#' 
#' @param dat ...
#' @param peptide_list ...
#' @param protein ...
#' @param time_0 ...
#' @param time_100 ...
#' @param deut_part ...
#' 
#' @details This is a wrapper for \code{\link{calculate_kinetics}}, but for
#' the peptide list instead of one peptide. The names of the parameters and 
#' variables will be changed later after the glossary project.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_kinetic_data_set

generate_kinetic_data_set <- function(dat,
                                      peptide_list,
                                      protein,
                                      time_0,
                                      time_100,
                                      deut_part){
  
  bind_rows(apply(peptide_list, 1, function(peptide){
    calculate_kinetics(dat = dat,
                       protein = protein, 
                       sequence = peptide[1],
                       state = peptide[2],
                       start = as.numeric(peptide[3]),
                       end = as.numeric(peptide[4]),
                       time_0 = time_0,
                       time_100 = time_100,
                       deut_part = 0.01*as.integer(deut_part))
  }))
  
}