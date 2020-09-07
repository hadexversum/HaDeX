#' Calculates confidence limits
#' 
#' @description Returns relation with confidence limits for each peptide.
#' 
#' @param calc_dat processed data from DynamX file - using \code{\link{prepare_dataset}}
#' @param confidence_limit confidence limit chosen by user - from range [0, 1]. 
#' @param theoretical logical value to determine if the plot is theoretical or not. 
#' @param relative logical value to determine if values are relative or absolute. 
#' 
#' @details ...
#' 
#' @return calc_dat extended by column specifying if given peptide is relevant in given confidence limit. 
#' The value of the confidence limit is added as an attribute - as well as parameters used to calculate (theoretical/relative)
#' 
#' @seealso \code{\link{read_hdx}} \code{\link{prepare_dataset}}
#' 
#' @examples 
#' #load example data
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'                             
#' # TODO
#'                      
#' 
#' @export add_stat_dependency

add_stat_dependency <- function(calc_dat,
                                confidence_limit = 0.98,
                                theoretical = FALSE,
                                relative = TRUE){
  
  value_column <- case_when(
    theoretical & relative ~ "diff_theo_frac_deut_uptake",
    theoretical & !(relative) ~ "diff_theo_deut_uptake",
    !(theoretical) & relative ~ "diff_frac_deut_uptake",
    !(theoretical) & !(relative) ~ "diff_deut_uptake"
  )
  
  confidence_values <- calculate_confidence_limit_values(calc_dat, 
                                                         confidence_limit = confidence_limit, 
                                                         relative = relative, 
                                                         theoretical = theoretical)
  
  calc_dat[[paste0("valid_at_", confidence_limit)]] <- calc_dat[[value_column]] > confidence_values[2] | calc_dat[[value_column]] < confidence_values[1]
  
  attr(calc_dat, paste0("confidence_limit_at_", confidence_limit)) <- confidence_values
  attr(calc_dat, paste0("confidence_limit_at_", confidence_limit, "_prop")) <- data.frame("theoretical" = theoretical, "relative" = relative)

  calc_dat
  
}