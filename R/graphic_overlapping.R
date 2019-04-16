#' Plot graphic sequence overlapping
#' 
#' Produces graphic sequence overlapping plot based on experimental HDX-MS data.
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
#' graphic_overlapping(dat, "CD160_HVEM")
#' 
#' @export graphic_overlapping
 
graphic_overlapping <- function(dat,
                                chosen_state){
  
  dat %>%
    select(Start, End, State) %>%
    filter(State %in% chosen_state) %>%
    select(-State) %>%
    filter(!duplicated(.)) %>%
    mutate(ID = 1L:nrow(.)) %>%
    melt(id.vars = "ID") %>%
    ggplot(aes(x = value, y = ID, group = ID)) +
    geom_line() +
    labs(title = 'Peptides positions compared to whole protein sequence',
         x = 'Position in sequence',
         y = '') +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
  
}
