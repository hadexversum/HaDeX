#' Plot peptide coverage
#' 
#' @description Plots the peptide coverage of the protein sequence.
#' 
#' @importFrom ggplot2 ggplot geom_line labs theme element_blank geom_rect
#' @importFrom data.table as.data.table setorderv
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
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{plot_position_frequency}}
#' 
#' @examples 
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
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
  
  dat <- as.data.table(dat)
  
  dat <- dat[Protein == protein & State %in% states, .(Start, End)]
  dat <- dat[!duplicated(dat)]
  dat[, Len := - End + Start]
  setorderv(dat, cols = c("Start", "Len"))
  
  levels <- rep(NA, (nrow(dat)))
  levels[1] <- 1
  
  start <- dat[["Start"]]
  end <- dat[["End"]]
  
  for(i in 1:(nrow(dat) - 1)) {
    
    for(level in 1:max(levels, na.rm = TRUE)) {
      
      if(all(start[i + 1] > end[1:i][levels == level] | end[i + 1] < start[1:i][levels == level], na.rm = TRUE)) {
        
        levels[i + 1] <- level
        break
        
      } else {
        if(level == max(levels, na.rm = TRUE)) {
          levels[i + 1] <- max(levels, na.rm = TRUE) + 1
        } 
      }
    }
  }
  
  dat[, ID := levels]
  
  if(show_blanks) {
    
    amino_dat <- data.table(amino_acids = unique(unlist(apply(dat, 
                                                       1, 
                                                       function(i) i[1]:i[2]))))
    
    missing <- data.frame(ids = setdiff(1:max(amino_dat[["amino_acids"]]), 
                                        amino_dat[["amino_acids"]]))
    
    non_missing <- data.frame(ids = amino_dat[["amino_acids"]])
    
    coverage_plot <- ggplot(data = dat) +
      geom_rect(missing,
                mapping = aes(xmin = ids - 0.5, 
                              xmax = ids + 0.5,
                              ymin = 0, 
                              ymax = max(dat[["ID"]])),
                fill = "#EFB0A1",
                colour = NA, 
                alpha = 0.6) +
      geom_rect(non_missing,
                mapping = aes(xmin = ids - 0.5, 
                              xmax = ids + 0.5,
                              ymin = 0, 
                              ymax = max(dat[["ID"]])),
                fill = "#C6EBBE",
                colour = NA, 
                alpha = 0.6)
  } else {
    coverage_plot <- ggplot(data = dat)
  }
  
  coverage_plot <- coverage_plot +
    geom_rect(data = dat, 
              mapping = aes(xmin = Start, 
                            xmax = End + 1, 
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
