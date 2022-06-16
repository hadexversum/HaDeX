#' Create differential dataset with statistical validity
#'
#' @param p_diff_uptake_dat ...
#' @param houde_interval ...
#' @param houde_interval_times ...
#' @param theoretical ...
#' @param fractional ...
#' 
#' @details ...
#' 
#' @return a \code{\link{data.frame}} object. 
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{create_p_diff_uptake_dataset}}
#' \code{\link{calculate_confidence_limit_values}}
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' p_diff_uptake_dat <- create_p_diff_uptake_dataset(dat)
#' p_diff_uptake_dat_confidence <- create_p_diff_uptake_dataset_with_confidence(p_diff_uptake_dat)
#' head(p_diff_uptake_dat_confidence)
#'
#' @export create_p_diff_uptake_dataset_with_confidence

create_p_diff_uptake_dataset_with_confidence <- function(p_diff_uptake_dat,
                                                         houde_interval = NULL,
                                                         houde_interval_times = NULL,
                                                         theoretical = F, 
                                                         fractional = F){
  
  confidence_level = attr(p_diff_uptake_dat, "confidence_level")
  
  if(is.null(houde_interval)){
    
    if(is.null(houde_interval_times)){ houde_interval_times <- unique(p_diff_uptake_dat[["Exposure"]]) }
    
    houde_interval <- p_diff_uptake_dat %>%
      filter(Exposure %in% houde_interval_times) %>% 
      calculate_confidence_limit_values(confidence_level = confidence_level,
                                        theoretical = theoretical, 
                                        fractional = fractional)
  }
  
  value <- case_when(
    theoretical & fractional ~ "diff_theo_frac_deut_uptake",
    theoretical & !(fractional) ~ "diff_theo_deut_uptake",
    !(theoretical) & fractional ~ "diff_frac_deut_uptake",
    !(theoretical) & !(fractional) ~ "diff_deut_uptake"
  )
  
  alpha <- -log(1 - confidence_level)
  
  p_diff_uptake_dat[["valid"]] <- p_diff_uptake_dat[["log_p_value"]] > alpha & abs(p_diff_uptake_dat[[value]]) > houde_interval[2]
  
  attr(p_diff_uptake_dat, "houde_interval") <- houde_interval
  attr(p_diff_uptake_dat, "houde_interval_times") <- houde_interval_times
  attr(p_diff_uptake_dat, "theoretical") <- theoretical
  attr(p_diff_uptake_dat, "fractional") <- fractional
  attr(p_diff_uptake_dat, "has_modification") <- attr(dat, "has_modification") 
  
  return(p_diff_uptake_dat)
  
}