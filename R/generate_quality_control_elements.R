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


#' generate_quality_control_plot
#' 
#' @description Generates quality control plot based on supplied data.
#' 
#' @param dat produced by \code{\link{quality_control}} function, 
#' scaled if necessary.
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_quality_control_plot

generate_quality_control_plot <- function(dat){
  
  dat %>%
    gather(2:7, key = 'type', value = 'value') %>%
    filter(startsWith(type, "avg")) %>%
    ggplot(aes(x = out_time, y = value, group = type)) +
    geom_point(size = 3) +
    geom_line(aes(color = type)) +
    scale_colour_discrete(name = "Mean uncertainty of: ", labels = c("difference", "first state", "second state")) +
    scale_x_log10() + 
    labs(x = "Out time [min]",
         y = "Mean uncertainty [%]",
         title = "Quality control plot for experiment")
  
}