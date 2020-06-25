#' generate_quality_control_data
#' 
#' @description  Generates the quality control data in user-friendly way.
#' 
#' @param dat data frame from \code{\link{quality_control}}, 
#' scaled if necessary.
#' 
#' @details This data is available from the GUI. May be internal.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_quality_control_data

generate_quality_control_data <- function(dat){
  
  dat %>%
    select(out_time, avg_err_state_first, avg_err_state_second, avg_diff) %>%
    mutate(avg_err_state_first = round(avg_err_state_first, 2),
           avg_err_state_second = round(avg_err_state_second, 2),
           avg_diff = round(avg_diff, 2)) %>%
    dt_format(cols = c("Out time", "Mean error - first state [%]", "Mean error - second state [%]", "Mean error of difference [%]"))
  
}