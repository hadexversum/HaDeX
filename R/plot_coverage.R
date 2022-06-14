#' Plot peptide coverage
#' 
#' @description Plots the peptide coverage of the protein sequence.
#' 
#' @importFrom ggplot2 ggplot geom_line labs theme element_blank
#' @importFrom plyr . 
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' @param protein protein to be included in plot
#' @param states sequence states to be included in plot
#' 
#' @details The function \code{plot_coverage} plots sequence coverage based on 
#' experimental data for chosen protein in chosen state. Only non-duplicated 
#' peptides are shown on the plot, next to each other. 
#' 
#' The aim of this plot is to see the dependence between positions of the 
#' peptides on the protein sequence. Their position in y-axis does not contain 
#' any information. 
#' 
#' @return a \code{\link[ggplot2]{ggplot}} object.
#' 
#' @seealso \code{\link{read_hdx}} \code{\link{plot_position_frequency}}
#' 
#' @examples 
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'                             
#' # plot coverage with default parameters
#' plot_coverage(dat)
#' 
#' # plot coverage with explicit parameters
#' plot_coverage(dat, protein = "db_CD160", states = "CD160_HVEM")
#' 
#' @export plot_coverage

plot_coverage <- function(dat, 
                          protein = dat[["Protein"]][1],
                          states = dat[["State"]][1],
                          show_blanks = TRUE){
  
  dat <- dat[Protein == protein & State %in% states, .(Start, End)]
  dat <- dat[!duplicated(dat)]
  setorderv(dat, cols = c("Start", "End"))
  
  levels <- rep(1, (nrow(dat_tmp)))
  for(i in 1:(nrow(dat_tmp) - 1)) {
    if(max(dat_tmp[1:i, "End"]) > dat_tmp[i + 1, "Start"]) {
      levels[i + 1] <- levels[i] + 1
    }
  }
  
  dat[, ID := levels]
  
  if(show_blanks) {
    
    amino_dat <- data.table(amino_acids = unique(unlist(apply(dat, 
                                                       1, 
                                                       function(i) i[1]:i[2]))))
    
    missing <- data.frame(ids = setdiff(1:max(amino_dat$amino_acids), 
                                        amino_dat$amino_acids))
    
    non_missing <- data.frame(ids = amino_dat$amino_acids)
    
    
    coverage_plot <- ggplot(data = dat) +
      geom_rect(missing,
                mapping = aes(xmin = ids - 0.5, 
                              xmax = ids + 0.5,
                              ymin = 0, 
                              ymax = max(data_plot[["ID"]])),
                fill = "#EFB0A1",
                colour = NA, 
                alpha = 0.6) +
      geom_rect(non_missing,
                mapping = aes(xmin = ids - 0.5, 
                              xmax = ids + 0.5,
                              ymin = 0, 
                              ymax = max(data_plot[["ID"]])),
                fill = "#C6EBBE",
                colour = NA, 
                alpha = 0.6)
  } else {
    coverage_plot <- ggplot(data = dat)
  }
  
  coverage_plot <- coverage_plot +
    geom_rect(data_plot, 
              mapping = aes(xmin = Start, 
                            xmax = End, 
                            ymin = ID, 
                            ymax = ID - 1), 
              fill = "#5A748C",
              colour = "black", 
              alpha = 0.8) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    labs(title = "Peptide coverage",
         x = "Position",
         y = "")
  
  return(HaDeXify(coverage_plot))
  
}
