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

#'
#' @description internal function for selection of the 
#' value label. Similar to get_uptake_name, but returns the 
#' user-friendly name used in e.q. table column names in GUI.
#' 
#' @param differential indicator
#' @param fractional indicator
#' @param theoretical indicator
#' 
#' 
#' @examples
#' get_uptake_label(differential = T, fractional = T, theoretical = T)
#' get_uptake_label(differential = F, fractional = T, theoretical = F)
#'
#' @noRd
#' 
get_uptake_label <- function(differential = FALSE, 
                             fractional = TRUE, 
                             theoretical = FALSE){
  
  value <- "DU"
  
  if(fractional) value <- paste0("Frac ", value,  " [%]")
  else value <- paste0(value, " [Da]")
  
  if(theoretical) value <- paste0("Theo ", value)
  
  if(differential) value <- paste0("Diff ", value)
  
  return(value)
  
}

#' @description internal function for selection of the 
#' value error label. Similar to get_uptake_error_name, but returns the 
#' user-friendly name used in e.q. table column names in GUI.
#' 
#' @param value used - if null, found using the parameters
#' @param differential indicator
#' @param fractional indicator
#' @param theoretical indicator
#' 
#' @examples
#' get_uptake_error_label(value = "deut_uptake")
#' get_uptake_error_label(differential = T, fractional = T, theoretical = T)
#' get_uptake_error_label(differential = F, fractional = T, theoretical = F)
#'
#' @noRd
get_uptake_error_label <- function(value = NULL, 
                                  differential = FALSE,
                                  fractional = TRUE, 
                                  theoretical = FALSE){
  
  if(is.null(value)){
    value <- get_uptake_name(differential = differential,
                             fractional = fractional, 
                             theoretical = theoretical)
  }
  
  return(paste0("Err(", value, ")"))
  
}