#' Plot peptide coverage
#' 
#' Plots the peptide coverage of the protein sequence.
#' 
#' @importFrom ggplot2 ggplot geom_line
#' @importFrom reshape2 melt
#' @importFrom dplyr filter
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' @param chosen_state sequence states to be included in plot
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' plot_coverage(dat)
#' plot_coverage(dat, chosen_state = "CD160_HVEM")
#' 
#' @export plot_coverage
 
plot_coverage <- function(dat, chosen_state = dat[["State"]][1]){
  
  dat %>%
    select(Start, End, State) %>%
    filter(State %in% chosen_state) %>%
    select(-State) %>%
    filter(!duplicated(.)) %>%
    mutate(ID = 1L:nrow(.)) %>%
    melt(id.vars = "ID") %>%
    ggplot(aes(x = value, y = ID, group = ID)) +
    geom_line() +
    labs(title = 'Peptide coverage',
         x = 'Position',
         y = '') +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
  
}
