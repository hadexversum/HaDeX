#' Calculate the value of confidence limit
#' 
#' @description Calculates confidence limit values for prepared dataset, based on chosen parameters.
#' 
#' @importFrom dplyr case_when
#' @importFrom stats qt
#' 
#' @param calc_dat processed data from DynamX file - using prepare_dataset
#' @param confidence_level confidence limit chosen by user - from range [0, 1]. 
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' 
#' @references Houde, D., Berkowitz, S.A., and Engen, J.R. (2011). 
#' The Utility of Hydrogen/Deuterium Exchange Mass Spectrometry in 
#' Biopharmaceutical Comparability Studies. J Pharm Sci 100, 2071–2086.
#' 
#' @details ...
#' 
#' @return range of confidence limit interval 
#' 
#' @seealso \code{\link{read_hdx}} \code{\link{prepare_dataset}} 
#' 
#' @examples 
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' #ToDO
#'                               
#' @export calculate_confidence_limit_values

calculate_confidence_limit_values <- function(calc_dat,
                                              confidence_level = 0.98,
                                              theoretical = FALSE,
                                              fractional = TRUE) {
  
  alpha <- 1 - confidence_level
  t_value <- qt(c(alpha/2, 1-alpha/2), df = 2)[2]
  
  err_column <- case_when(
    theoretical & fractional ~ "err_diff_theo_frac_deut_uptake",
    theoretical & !(fractional) ~ "err_diff_theo_deut_uptake",
    !(theoretical) & fractional ~ "err_diff_frac_deut_uptake",
    !(theoretical) & !(fractional) ~ "err_diff_deut_uptake"
  )
  
  confidence_limit_value <- t_value * mean(calc_dat[[err_column]], na.rm = TRUE)/sqrt(length(calc_dat))
  
  c(-confidence_limit_value, confidence_limit_value)
 
}