#' Plot peptide coverage
#' 
#' @description Plots the peptide coverage of the protein sequence.
#' 
#' @importFrom ggplot2 ggplot geom_line
#' @importFrom reshape2 melt
#' @importFrom dplyr filter
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' @param protein protein to be included in plot
#' @param chosen_state sequence states to be included in plot
#' 
#' @examples 
#' 
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'                             
#' # plot coverage with default parameters
#' plot_coverage(dat)
#' 
#' # plot coverage with explicit parameters
#' plot_coverage(dat, protein = "db_CD160", chosen_state = "CD160_HVEM")
#' 
#' @details Function \code{plot_coverage} plots sequence coverage based on experimental data for chosen protein in chosen state. 
#' 
#' @return a ggplot object that can be further modified according to the user needs.
#' 
#' @seealso \code{\link{read_hdx}}
#' 
#' @export plot_coverage
 
plot_coverage <- function(dat, 
                          protein = dat[["Protein"]][1],
                          chosen_state = dat[["State"]][1]){
  
  dat %>%
    filter(Protein == protein) %>%
    select(Start, End, State) %>%
    filter(State %in% chosen_state) %>%
    select(-State) %>%
    filter(!duplicated(.)) %>%
    mutate(ID = 1L:nrow(.)) %>%
    melt(id.vars = "ID") %>%
    ggplot(aes(x = value, y = ID, group = ID)) +
    geom_line() +
    labs(title = "Peptide coverage",
         x = "Position",
         y = "") +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank())
  
}
