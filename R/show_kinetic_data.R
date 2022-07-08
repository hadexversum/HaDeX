#' Shows selected kinetics data
#' 
#' @description Generates deuterium uptake data, based on the supplied
#' parameters.
#' 
#' @param kin_dat calculated kinetic data by \code{\link{calculate_kinetics}} 
#' or \code{\link{calculate_peptide_kinetics}} or \code{\link{create_kinetic_dataset}}
#' function.
#' @param theoretical \code{logical}, determines if plot shows theoretical values.
#' @param fractional \code{logical}, determines if plot shows fractional values.
#' 
#' @details This data is available in the GUI. 
#' All of the numerical values are rounded to 4 places after the dot!!
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_kinetics}}
#' \code{\link{calculate_peptide_kinetics}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'
#' # one peptide in one state
#' kin1 <- calculate_kinetics(dat, 
#'                            protein = "db_CD160",
#'                            sequence = "INITSSASQEGTRLN", 
#'                            state = "CD160",
#'                            start = 1, 
#'                            end = 15,
#'                            time_0 = 0.001, 
#'                            time_100 = 1440)
#' show_kinetic_data(kin1)
#' 
#' @export show_kinetic_data

show_kinetic_data <- function(kin_dat, 
                              theoretical = FALSE, 
                              fractional = FALSE){
  
  kin_dat <- data.table(kin_dat)
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional
      tmp <- kin_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         theo_frac_deut_uptake, err_theo_frac_deut_uptake)]
      tmp[, `:=`(theo_frac_deut_uptake = round(theo_frac_deut_uptake, 4),
                 err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "theo_frac_deut_uptake", 
                      "err_theo_frac_deut_uptake"),
               c("Time Point", "Theo Frac DU [%]", "Theo Err Frac DU [%]"))
      
    } else {
      # theoretical & absolute
      tmp <- kin_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         theo_deut_uptake, err_theo_deut_uptake)]
      tmp[, `:=`(theo_deut_uptake = round(theo_deut_uptake, 4),
                 err_theo_deut_uptake = round(err_theo_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "theo_deut_uptake", "err_theo_deut_uptake"),
               c("Time Point", "Theo DU [Da]", "Theo Err DU [Da]"))
      
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      tmp <- kin_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         frac_deut_uptake, err_frac_deut_uptake)]
      tmp[, `:=`(frac_deut_uptake = round(frac_deut_uptake, 4),
                 err_frac_deut_uptake = round(err_frac_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "frac_deut_uptake", "err_frac_deut_uptake"),
               c("Time Point", "Frac DU [%]", "Err Frac DU [%]"))
      
    } else {
      # experimental & absolute
      tmp <- kin_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         deut_uptake, err_deut_uptake)]
      tmp[, `:=`(deut_uptake = round(deut_uptake, 4),
                 err_deut_uptake = round(err_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "deut_uptake", "err_deut_uptake"),
               c("Time Point", "DU [Da]", "Err DU [Da]"))
    }
  }
}