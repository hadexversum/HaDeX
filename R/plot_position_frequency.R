#' Plot position frequency
#' 
#' Plots the frequency of positions
#' 
#' @importFrom ggplot2 ggplot geom_line
#' @importFrom reshape2 melt
#' @importFrom dplyr filter summarise
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' @param chosen_state sequence states to be included in plot
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' plot_position_frequency(dat, chosen_state = "CD160_HVEM")
#' 
#' @export 

plot_position_frequency <- function(dat, chosen_state = dat[["State"]][1]) {

  coverage_df <- dat %>%
    select(Start, End, State) %>% 
    filter(State == chosen_state) %>% 
    filter(!duplicated(.)) %>% 
    select(-State) %>% 
    apply(1, function(i) i[1]:i[2]) %>% 
    unlist %>% 
    data.frame(x = .) %>% 
    group_by(x) %>% 
    summarise(coverage = length(x)) 
  
  ggplot(coverage_df, aes(x = x, y = coverage)) +
    geom_col(width = 1) +
    labs(x = 'Position', y = 'Position frequency in peptides') +
    theme(legend.position = "none")
}
