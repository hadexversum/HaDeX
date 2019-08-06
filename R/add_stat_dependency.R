#' Calculates confidence limits
#' 
#' @description Returns relation with confidence limits for each peptide.
#' 
#' @param calc_dat processed data from DynamX file - using \code{\link{prepare_dataset}}
#' @param confidence_limit confidence limit chosen by user - from range [0, 1]. 
#' @param theoretical logical value to determine if plot is theoretical or not. 
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
#' # prepate dataset for states `CD160` and `CD160_HVEM` in given time parameters 
#' calc_dat <- prepare_dataset(dat,
#'                             in_state_first = "CD160_0.001",
#'                             chosen_state_first = "CD160_1",
#'                             out_state_first = "CD160_1440",
#'                             in_state_second = "CD160_HVEM_0.001",
#'                             chosen_state_second = "CD160_HVEM_1",
#'                             out_state_second = "CD160_HVEM_1440") 
#'                             
#' # add calculated confidence limits for prepared data
#' add_stat_dependency(calc_dat, 
#'                     confidence_limit = 0.98, 
#'                     theoretical = FALSE, 
#'                     relative = TRUE)
#'                      
#' 
#' @export add_stat_dependency

add_stat_dependency <- function(calc_dat,
                                confidence_limit = 0.98,
                                theoretical = FALSE,
                                relative = TRUE){
  
  value_column <- case_when(
    theoretical & relative ~ "diff_theo_frac_exch",
    theoretical & !(relative) ~ "abs_diff_theo_frac_exch",
    !(theoretical) & relative ~ "diff_frac_exch",
    !(theoretical) & !(relative) ~ "abs_diff_frac_exch"
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