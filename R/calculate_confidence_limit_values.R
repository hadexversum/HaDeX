#' Calculate the value of confidence limit
#' 
#' @description Calculates confidence limit values for prepared provided, 
#' based on chosen parameters.
#' 
#' @importFrom dplyr case_when coalesce
#' @importFrom stats qt
#' 
#' @param diff_uptake_dat processed data from DynamX file - using prepare_dataset
#' @param confidence_level confidence level for the test, from range [0, 1].
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' 
#' @references Houde, D., Berkowitz, S.A., and Engen, J.R. (2011). 
#' The Utility of Hydrogen/Deuterium Exchange Mass Spectrometry in 
#' Biopharmaceutical Comparability Studies. J Pharm Sci 100, 2071–2086.
#' 
#' @details Function \code{\link{calculate_confidence_limit_values}} 
#' calculates confidence limit using Houde test. The confidence limits 
#' are calculated on whole provided dataset. If the user wishes to calculate
#' confidence limit for one, two or more time points, the provided data 
#' should be adjusted accordingly. 
#' 
#' @return range of confidence limit interval.
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{calculate_diff_uptake}} 
#' \code{\link{create_diff_uptake_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- calculate_diff_uptake(dat)
#' calculate_confidence_limit_values(diff_uptake_dat)
#'                               
#' @export calculate_confidence_limit_values

calculate_confidence_limit_values <- function(diff_uptake_dat,
                                              confidence_level = 0.98,
                                              theoretical = FALSE,
                                              fractional = TRUE,
                                              n_rep = NA) {
  

  n_rep <- fcoalesce(c(attr(diff_uptake_dat, "n_rep"), n_rep, 3))[1]

  alpha <- 1 - confidence_level
  t_value <- qt(c(alpha/2, 1-alpha/2), df = n_rep-1)[2]
  
  err_column <- fcase(
    theoretical & fractional, "err_diff_theo_frac_deut_uptake",
    theoretical & !(fractional), "err_diff_theo_deut_uptake",
    !(theoretical) & fractional, "err_diff_frac_deut_uptake",
    !(theoretical) & !(fractional),"err_diff_deut_uptake"
  )
  
  if(is.null((diff_uptake_dat[[err_column]]))){ err_column = "err_value" }
  
  confidence_limit_value <- t_value * mean(diff_uptake_dat[[err_column]], na.rm = TRUE)/sqrt(length(diff_uptake_dat))
  
  c(-confidence_limit_value, confidence_limit_value)
 
}