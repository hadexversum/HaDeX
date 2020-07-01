#' generate_overlap_plot
#' 
#' @description Generates overapping peptide plot based on supplied data
#' and parameters. 
#' 
#' @param dat produced by \code{\link{generate_overlap_plot}}
#' function
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_overlap_plot

generate_overlap_plot <- function(dat){
  
  dat %>%
    select(Sequence, Start, End) %>%
    filter(!duplicated(.)) %>%
    arrange(Start, End) %>%
    mutate(ID = row_number()) %>%
    ggplot() +
    geom_segment(aes(x = Start, y = ID, xend = End, yend = ID)) +
    labs(title = "Peptide coverage",
         x = "Position",
         y = "") +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) 
}