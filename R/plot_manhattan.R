#' Manhattan plot
#' 
#' @param p_dat data produced by the \code{\link{create_p_diff_uptake_dataset}}
#' function.
#' @param plot_title title for the plot. If not provided, it is constructed in a form:
#' "Differences between state_1 and state_2"
#' @param separate_times \code{logical}, indicates if the data should be seen on the same plot, 
#' or on separate plots for each time point of measurement.
#' @param times vector of time points of measurements to be included in the plot.
#' @param confidence_level confidence level for the test, from range [0, 1]. 
#' @param show_confidence_limit logical, indicates if the hybrid testing
#' confidence intervals are shown.
#' @param show_peptide_position \code{logical}, indicates if the peptide length
#' and position in  the  sequence is shown. Otherwise, the peptides are represented by their ID.
#' 
#' @details ...
#'  
#' @references Hageman, T. S. & Weis, D. D. Reliable Identification of Significant
#' Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements
#' Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{create_p_diff_uptake_dataset}}
#' 
#' @examples
#' dat <-  read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' p_dat <- create_p_diff_uptake_dataset(dat)
#' plot_manhattan(p_dat)
#' 
#' plot_manhattan(p_dat, separate_times = F)
#' 
#' plot_manhattan(p_dat, show_peptide_position = T, separate_times = F)
#' 
#' plot_manhattan(p_dat, separate_times = F, show_confidence_limit = F)
#'  
#' @export plot_manhattan
   
plot_manhattan <- function(p_dat,
                           plot_title = NULL, 
                           separate_times = T,
                           times = NULL,
                           confidence_level = NULL,
                           show_confidence_limit = T,
                           show_peptide_position = F){
  
  p_dat <- as.data.table(p_dat)
  
  if(is.null(confidence_level)) {confidence_level <- attr(p_dat, "confidence_level") }

  if(is.null(times)) { times <- unique(p_dat[["Exposure"]])}
  
  plot_dat <- p_dat[Exposure %in% times]
   
  confidence_limit <- -log(1 - confidence_level)
  
  if(is.null(plot_title)) {plot_title <- paste0("Differences between ", attr(p_dat, "state_1"), " and ", attr(p_dat, "state_2")) }
  
  manhattan_plot <- ggplot(plot_dat) + 
    theme(legend.position = "bottom") +
    labs(title = plot_title,
         y = "log(P value)", 
         color = "Exposure")
  
  if(show_peptide_position){
    
    manhattan_plot <- manhattan_plot +
      geom_segment(aes(x = Start, y = log_p_value, xend = End, yend = log_p_value, color = as.factor(Exposure))) +
      labs(x = "Peptide position")
    
  } else{
    
    manhattan_plot <- manhattan_plot +
      geom_point(aes(x = ID, y = log_p_value, color = as.factor(Exposure))) +
      labs(x = "Peptide ID")
  }
  
  if(separate_times){
    
    manhattan_plot <- manhattan_plot + 
      facet_wrap(~ as.factor(Exposure)) +
      theme(legend.position = "none")
    
  } 
  
  if(show_confidence_limit){ 
    
    manhattan_plot <- manhattan_plot +
      geom_hline(yintercept = confidence_limit, linetype = "dashed") 
  }
  
  return(HaDeXify(manhattan_plot))
  
}
