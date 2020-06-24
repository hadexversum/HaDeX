#' generate_kinetic_data_set
#' 
#' @description ...
#' 
#' @param dat ...
#' 
#' @details ...
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