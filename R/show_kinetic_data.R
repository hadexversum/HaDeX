#' Deuterium uptake curve data
#' 
#' @description Present deuterium uptake curve data
#' 
#' @param uc_dat calculated kinetic data by \code{\link{calculate_kinetics}} 
#' or \code{\link{calculate_peptide_kinetics}} or 
#' \code{\link{create_kinetic_dataset}} function
#' @param theoretical \code{logical}, indicator if values are 
#' calculated using theoretical controls
#' @param fractional \code{logical}, indicator if values are shown 
#' in fractional form 
#' 
#' @details The function \code{\link{show_uptake_data}} generates a subsets
#' of the uc_dat based on selected parameters.
#' The numerical values are rounded to 4 places. The names of columns
#' are changed to user-friendly ones. 
#' 
#' @return a \code{\link{data.frame}} object
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_kinetics}}
#' \code{\link{calculate_peptide_kinetics}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' uc_dat <- calculate_kinetics(dat, protein = "db_CD160",
#'                              sequence = "INITSSASQEGTRLN", 
#'                              state = "CD160",
#'                              start = 1, end = 15,
#'                              time_0 = 0.001, time_100 = 1440)
#' show_uc_data(uc_dat)
#' 
#' @export show_uc_data

show_uc_data <- function(uc_dat, 
                         theoretical = FALSE, 
                         fractional = FALSE){
  
  uc_dat <- as.data.table(uc_dat)
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional
      tmp <- uc_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         theo_frac_deut_uptake, err_theo_frac_deut_uptake)]
      tmp[, `:=`(theo_frac_deut_uptake = round(theo_frac_deut_uptake, 4),
                 err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "theo_frac_deut_uptake", 
                      "err_theo_frac_deut_uptake"),
               c("Time Point", "Theo Frac DU [%]", "Theo Err Frac DU [%]"))
      
    } else {
      # theoretical & absolute
      tmp <- uc_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         theo_deut_uptake, err_theo_deut_uptake)]
      tmp[, `:=`(theo_deut_uptake = round(theo_deut_uptake, 4),
                 err_theo_deut_uptake = round(err_theo_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "theo_deut_uptake", "err_theo_deut_uptake"),
               c("Time Point", "Theo DU [Da]", "Theo Err DU [Da]"))
      
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      tmp <- uc_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         frac_deut_uptake, err_frac_deut_uptake)]
      tmp[, `:=`(frac_deut_uptake = round(frac_deut_uptake, 4),
                 err_frac_deut_uptake = round(err_frac_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "frac_deut_uptake", "err_frac_deut_uptake"),
               c("Time Point", "Frac DU [%]", "Err Frac DU [%]"))
      
    } else {
      # experimental & absolute
      tmp <- uc_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         deut_uptake, err_deut_uptake)]
      tmp[, `:=`(deut_uptake = round(deut_uptake, 4),
                 err_deut_uptake = round(err_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "deut_uptake", "err_deut_uptake"),
               c("Time Point", "DU [Da]", "Err DU [Da]"))
    }
  }
  
  return(tmp)
}