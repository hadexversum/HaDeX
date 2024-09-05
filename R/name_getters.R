#'
#' @description internal function for selection of the 
#' value name.
#' 
#' @param differential indicator
#' @param fractional indicator
#' @param theoretical indicator
#' 
#' @examples
#' get_uptake_name(differential = T, fractional = T, theoretical = T)
#' get_uptake_name(differential = F, fractional = T, theoretical = F)
#'
#' @noRd

get_uptake_name <- function(differential = FALSE, 
                            fractional = TRUE, 
                            theoretical = FALSE){
  
  value <- "deut_uptake"
  
  if(fractional) value <- paste0("frac_", value)
  
  if(theoretical) value <- paste0("theo_", value)
  
  if(differential) value <- paste0("diff_", value)
  
  return(value)
  
}

#' @description internal function for selection of the 
#' value error name.
#' 
#' @param value used - if null, found using the parameters
#' @param differential indicator
#' @param fractional indicator
#' @param theoretical indicator
#' 
#' @examples
#' get_uptake_error_name(value = "deut_uptake")
#' get_uptake_error_name(differential = T, fractional = T, theoretical = T)
#' get_uptake_error_name(differential = F, fractional = T, theoretical = F)
#'
#' @noRd
get_uptake_error_name <- function(value = NULL, 
                                  differential = FALSE,
                                  fractional = TRUE, 
                                  theoretical = FALSE){
  
  if(is.null(value)){
    value <- get_uptake_name(differential = differential,
                             fractional = fractional, 
                             theoretical = theoretical)
  }
  
  return(paste0("err_", value))
  
}
