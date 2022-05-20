#' Calculates confidence limits
#' 
#' @description Returns relation with confidence limits for each peptide.
#' 
#' @param calc_dat data produced by \code{\link{generate_differential_data_set}}
#' funcion.
#' @param confidence_level confidence limit - from range [0, 1]. 
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' 
#' @details This function checks if the values are statistically significant based 
#' on provided criteria using Houde test. 
#' 
#' @return calc_dat extended by column specifying if given peptide is relevant in
#' given confidence limit. The value of the confidence limit is added as an attribute 
#' - as well as parameters used to calculate (theoretical/fractional).
#' 
#' @references Houde, D., Berkowitz, S.A., and Engen, J.R. (2011). 
#' The Utility of Hydrogen/Deuterium Exchange Mass Spectrometry in 
#' Biopharmaceutical Comparability Studies. J Pharm Sci 100, 2071–2086.
#' 
#' @seealso \code{\link{read_hdx}} \code{\link{calculate_diff_uptake}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' calc_dat <- calculate_diff_uptake(dat)
#' result <- add_stat_dependency(calc_dat)
#' head(result)                            
#' 
#' 
#' @export add_stat_dependency

add_stat_dependency <- function(calc_dat,
                                confidence_level = 0.98,
                                theoretical = FALSE, 
                                fractional = TRUE){
  
  options <- c("diff_theo_frac_deut_uptake",
               "diff_theo_deut_uptake",
               "diff_frac_deut_uptake",
               "diff_deut_uptake")
  
  value_column <- options[c(theoretical & fractional,
                            theoretical & !(fractional),
                            !(theoretical) & fractional,
                            !(theoretical) & !(fractional))]
  
  confidence_values <- calculate_confidence_limit_values(calc_dat,
                                                         confidence_level = confidence_level,
                                                         fractional = fractional,
                                                         theoretical = theoretical)
  
  calc_dat[[paste0("valid_at_", confidence_level)]] <- calc_dat[[value_column]] > confidence_values[2] | calc_dat[[value_column]] < confidence_values[1]
  
  attr(calc_dat, paste0("confidence_limit_at_", confidence_level)) <- confidence_values
  attr(calc_dat, paste0("confidence_limit_at_", confidence_level, "_prop")) <- data.frame("theoretical" = theoretical, "fractional" = fractional)
  
  calc_dat
  
}