#' Position frequency
#' 
#' @description Plot the frequency of coverage for protein sequence
#' 
#' @importFrom ggplot2 ggplot geom_line geom_col geom_density
#' @importFrom data.table setnames merge.data.table tstrsplit
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' @param protein selected protein
#' @param state selected biological state for given protein
#' 
#' @details The function \code{plot_position_frequency} generates a 
#' histogram of the coverage frequency in selected biological states
#' for selected protein. 
#' The position frequency plot presents how many times each position of 
#' the sequence is covered by different peptides.
#' 
#' @return a \code{\link{ggplot}} object
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{plot_coverage}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' plot_position_frequency(dat)
#' plot_position_frequency(dat, state = "CD160_HVEM")
#' 
#' @export plot_position_frequency

plot_position_frequency <- function(dat, 
                                    protein = dat[["Protein"]][1],
                                    state = dat[["State"]][1]) {
  dat <- as.data.table(dat)
    
  dat <- dat[Protein == protein & State == state, .(Start, End)]
  dat <- dat[!duplicated(dat)]
  setorderv(dat, cols = c("Start", "End"))
  
  aminos_dat <- unlist(apply(dat, 1, function(i) i[1]:i[2]))
  freq_dat <- data.table(freq = table(factor(aminos_dat, 
                                             levels = 1:max(dat[["End"]]))))
  setnames(freq_dat, c("freq.V1", "freq.N"), c("amino", "freq"))
  freq_dat[, amino := as.numeric(amino)]
  
  pos_freq_plot <- ggplot() +
    geom_density(mapping = aes(x = aminos_dat, y = ..count..),
                 fill = "#5A748C", 
                 col = NA,
                 alpha = 0.2) +
    geom_segment(freq_dat, 
                 mapping = aes(x = amino,
                               xend = amino, 
                               yend = freq,
                               y = 0), 
                 stat = "identity") +
    labs(x = 'Position', 
         y = 'Position frequency in peptides') +
    theme(legend.position = "none")
  
  return(HaDeXify(pos_freq_plot))
  
}
