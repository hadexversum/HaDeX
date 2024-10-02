#' Calculates the aggregated uptake for peptide pool
#'
#' @param kin_dat ...
#'
#' @examples
#' kin_dat <- create_uptake_dataset(alpha_dat, states = "Alpha_KSCN")
#' create_aggregated_uptake_dataset(kin_dat)
#'
#' @export

create_aggregated_uptake_dataset <- function(kin_dat){

  kin_dat <- as.data.table(kin_dat)
  times <- unique(kin_dat[["Exposure"]])

  uc_dataset <- lapply(times, function(time){

    uc_t <- calculate_aggregated_uptake(kin_dat,
                                        time_t = time)

  }) %>% bind_rows()

  return(uc_dataset)

}


#' Show aggregated values in friendly form
#' 
#' @param aggregated_dat ...
#' @param differential ...
#' @param fractional ...
#' @param theoretical ...
#' 
#' @description Function plots the aggregated uptake data
#' with regard to submitted parameters in a friendly form.
#' Designed for GUI.
#' 
#' @return a data.table object
#' 
#' @examples 
#' kin_dat <- create_uptake_dataset(alpha_dat, states = "Alpha_KSCN")
#' aggregated_dat <- create_aggregated_uptake_dataset(kin_dat)
#' show_aggregated_uptake_data(aggregated_dat)
#' 
#' diff_uptake_dat <- create_diff_uptake_dataset(alpha_dat)
#' aggregated_diff_dat <- create_aggregated_diff_uptake_dataset(diff_uptake_dat)
#' show_aggregated_uptake_data(aggregated_diff_dat, differential = TRUE)
#' 
#' @export

show_aggregated_uptake_data <- function(aggregated_dat, 
                                        differential = FALSE,
                                        fractional = TRUE,
                                        theoretical = FALSE){
  
  value <- get_uptake_name(fractional = fractional, 
                           theoretical = theoretical, 
                           differential = differential)
  
  err_value <- get_uptake_error_name(value = value)
  
  tmp_dat <- as.data.table(aggregated_dat)
  
  tmp_dat <- tmp_dat[, .(position, aa, Exposure, 
                         val = get(value), err_val = get(err_value))]
  
  tmp_dat[, `:=`(val = round(val, 4),
                 err_val = round(err_val, 4))]
  
  setnames(tmp_dat,
           c("val", "err_val"),
           c(value, err_value))
  
  tmp_dat <- as.data.frame(tmp_dat)
  
  return(tmp_dat)
  
}
