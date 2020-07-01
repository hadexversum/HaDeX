#' generate_overlap_distribution_plot
#' 
#' @description Generates overlap distribution plot based on supplied data
#' and parameters.
#' 
#' @param dat produced by \code{\link{generate_overlap_distribution_plot}}
#' function
#' @param start ...
#' @param end ...
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_overlap_distribution_plot

generate_overlap_distribution_plot <- function(dat,
                                               start,
                                               end){
  
  mean_coverage <- round(mean(dat[["coverage"]], na.rm = TRUE), 2)
  display_position <- (start + end)/2
  
  dat %>% 
    ggplot(aes(x = pos, y = coverage)) +
    geom_col(width = 1) +
    labs(x = 'Position', y = 'Position frequency in peptides') +
    theme(legend.position = "none") + 
    coord_cartesian(xlim = c(start, end)) +
    geom_hline(yintercept = mean_coverage, color = 'red') +
    geom_text(aes(x = display_position, y = mean_coverage), label = paste0("Average frequency: ", mean_coverage), color = 'red', vjust = -.5)
  
}