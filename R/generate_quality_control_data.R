#' generate_quality_control_data
#' 
#' @description  Generates quality control data, based on the supplied
#' parameters.
#' 
#' @param dat custom format, produced by \code{\link{quality_control}}, 
#' scaled if necessary.
#' 
#' @details This data is available in the GUI. The names of the parameters
#' and variables will be changed later after the glossary project.
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
    rename("Out time" = out_time,
           "Mean error - first state [%]" = avg_err_state_first,
           "Mean error - second state [%]" = avg_err_state_second,
           "Mean error of difference [%]" = avg_diff)
}