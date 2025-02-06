#' Coverage heatmap data
#' 
#' @param x_dat data created using calculate_ or create_ 
#' function
#' @param value value to be presented
#' 
#' @description This function prepares the data used in 
#' coverage heatmap to be shown in user-friendly way.
#' 
#' @examples
#' # auc data
#' auc_dat <- calculate_auc(create_uptake_dataset(dat))
#' show_coverage_heatmap_data(auc_dat, value = "auc")
#' 
#' # back-exchange
#' bex_dat <- calculate_back_exchange(dat, state = "CD160")
#' show_coverage_heatmap_data(bex_dat, value = "back_exchange")
#' 
#' @export show_coverage_heatmap_data

show_coverage_heatmap_data <- function(x_dat, 
                                       value = NULL){
  
  x_dat <- as.data.table(x_dat)
  
  if(value == "auc"){
      
    x_dat[, `:=`(auc = round(auc, 4))]
    setnames(x_dat, c("auc"), c("AUC"), skip_absent = TRUE)
    
  } else if (value == "back_exchange"){
    
    x_dat[, `:=`(back_exchange = round(back_exchange, 4),
                 err_back_exchange = round(err_back_exchange, 4))]
    setnames(x_dat, 
             c("back_exchange", "err_back_exchange"), 
             c("Back Exchange", "U(Back Exchange)"), 
             skip_absent = TRUE)
  } 
  
  return(as.data.frame(x_dat))
  
}