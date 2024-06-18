#' 
#' @export

create_aggregated_uptake_dataset <- function(kin_dat){
  
  kin_dat <- as.data.table(kin_dat)
  times <- unique(kin_dat[["Exposure"]])
  
  uc_dataset <- lapply(times, function(time){
    
    uc_t <- calculate_aggregated_uptake(kin_dat, 
                                        time_t = time)
    
    uc_t[["Exposure"]] <- time
    
    
  }) %>% bind_rows()
  
  return(uc_dataset)
  
}