#' @export

create_aggregated_differential_uptake_dataset <- function(diff_uptake_dat){
  
  diff_uptake_dat <- as.data.table(diff_uptake_dat)
  times <- unique(diff_uptake_dat[["Exposure"]])
  
  diff_uptake_dataset <- lapply(times, function(time){
    
    diff_uc_t <- calculate_aggregated_differential_uptake(diff_uptake_dat, 
                                                          time_t = time)
  }) %>% bind_rows()
  
  return(diff_uptake_dataset)
}