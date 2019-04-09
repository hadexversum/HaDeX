#' calculate_confidence_limit_values
#' 
#' Calculates confidence limit values for prepared datased, based on chosen parameters.
#' 
#' @param calc_dat processed data from DynamiX file - using prepare_dataset
#' @param confidence_limit confidence limit chosen by user - from range [0, 1]. default : 0.98
#' @param theoretical logical value to determine if plot is theoretical or not. default : false
#' @param relative logical value to determine if values are relative or absolute. default : true
#'
#' @return range of confidence limit interval 
#'  
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' calc_dat <- prepare_dataset(dat,
#'                             in_state_first = "CD160_0.001",
#'                             chosen_state_first = "CD160_1",
#'                             out_state_first = "CD160_1440",
#'                             in_state_second = "CD160_HVEM_0.001",
#'                             chosen_state_second = "CD160_HVEM_1",
#'                             out_state_second = "CD160_HVEM_1440")   
#' calculate_confidence_limit_values(calc_dat = calc_dat,
#'                                   confidence_limit = 0.99,
#'                                   theoretical = FALSE, 
#'                                   relative = TRUE)                          
#'                               
#' @export calculate_confidence_limit_values

calculate_confidence_limit_values <- function(calc_dat,
                                              confidence_limit = 0.98,
                                              theoretical = FALSE,
                                              relative = TRUE) {
  
  alpha <- 1- confidence_limit
  t_value <- qt(c(alpha/2, 1-alpha/2), df = 2)[2]
  
  err_column <- case_when(
    theoretical & relative ~ "err_diff_theo_frac_exch",
    theoretical & !(relative) ~ "err_abs_diff_theo_frac_exch",
    !(theoretical) & relative ~ "err_frac_exch",
    !(theoretical) & !(relative) ~ "err_abs_diff_frac_exch"
  )
  
  confidence_limit_value <- t_value * mean(calc_dat[[err_column]], na.rm = TRUE)/sqrt(length(calc_dat))
  
  c(-confidence_limit_value, confidence_limit_value)
  
}