#' Plot position frequency
#' 
#' @description Plots the frequency of coverage of protein sequence.
#' 
#' @importFrom ggplot2 ggplot geom_line geom_col
#' @importFrom reshape2 melt
#' @importFrom dplyr filter summarise
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' @param protein protein to be included in plot
#' @param chosen_state sequence states to be included in plot
#' 
#' @details The function \code{plot_position_frequency} plots a histogram of the coverage frequency based on experimental data.
#' The aim of this plot is to see how many times each position of the sequence was covered (by different peptides).
#' 
#' @return a \code{\link[ggplot2]{ggplot}} object.
#' 
#' @seealso \code{\link{read_hdx}} \code{\link{plot_coverage}}
#' 
#' @examples 
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'                             
#' # function call with default parameters
#' plot_position_frequency(dat)
#' 
#' # function call with explicit parameters
#' plot_position_frequency(dat, chosen_state = "CD160_HVEM")
#' 
#' @export plot_position_frequency

plot_position_frequency <- function(dat, 
                                    protein = dat[["Protein"]][1],
                                    chosen_state = dat[["State"]][1]) {

  coverage_df <- dat %>%
    filter(Protein == protein) %>%
    select(Start, End, State) %>% 
    filter(State == chosen_state) %>% 
    filter(!duplicated(.)) %>% 
    select(-State) %>% 
    apply(1, function(i) i[1]:i[2]) %>% 
    unlist %>% 
    data.frame(x = .) %>% 
    group_by(x) %>% 
    summarise(coverage = length(x)) 
  
  pos_freq_plot <- ggplot(coverage_df, aes(x = x, y = coverage)) +
    geom_col(width = 1) +
    labs(x = 'Position', y = 'Position frequency in peptides') +
    theme(legend.position = "none")
  
  return(HaDeXify(pos_freq_plot))
}
