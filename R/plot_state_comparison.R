#' Plot state comparison
#' 
#' @importFrom ggplot2 ggplot geom_segment geom_errorbar theme scale_y_continuous
#' 
#' @param dat data produced by \code{\link{calculate_state_uptake}} function.
#' @param theoretical \code{logical}, determines if values are theoretical. 
#' @param fractional \code{logical}, determines if values are fractional.
#' 
#' @details Function \code{\link{plot_state_comparison}} presents provided 
#' data in a form of comparison plot, for peptides for chosen protein in chosen states,
#' at one time point of measurement at once. On X-axis there is a position in a sequence, 
#' with length of a segment of each peptide representing its length. On Y-axis there 
#' is deuterium uptake in chosen form. Error bars represents the combined and propagated
#' uncertainty. 
#' This plot is visible in GUI. 
#' 
#' @return a \code{ggplot} object.
#' 
#' @seealso 
#' \code{\link{calculate_state_uptake}}  
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' comparison_dat <- calculate_state_uptake(dat)
#' plot_state_comparison(comparison_dat)
#' 
#' @export plot_state_comparison

plot_state_comparison <- function(dat, 
                                  theoretical = FALSE, 
                                  fractional = FALSE){
  
  if (theoretical) {
    
    title <- "Theoretical comparison plot"
    
    if (fractional) {
      
      # theoretical & fractional
      value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      y_label <- "fractional deuterium uptake [%]"
      
    } else {
      
      # theoretical & absolute
      value <- "theo_deut_uptake"
      err_value <- "err_theo_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      
    } 
    
  } else {
    
    title <- "Comparison plot"
    
    if (fractional) {
      
      # experimantal & fractional
      value <- "frac_deut_uptake"
      err_value <- "err_frac_deut_uptake"
      y_label <- "fractional deuterium uptake [%]"
      
    } else {
      
      # experimental & absolute 
      value <- "deut_uptake"
      err_value <- "err_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      
    }
    
  }
  
  plot_dat <- data.frame(Sequence = dat[["Sequence"]],
                         Start = dat[["Start"]],
                         End = dat[["End"]],
                         Med_Sequence = dat[["Med_Sequence"]],
                         State = dat[["State"]],
                         value = dat[[value]],
                         err_value = dat[[err_value]])
  
  state_comp_plot <- ggplot(data = plot_dat) +
    geom_segment(data = plot_dat, aes(x = Start, y = value, xend = End, yend = value, color = State)) +
    geom_errorbar(data = plot_dat, aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = State)) +
    labs(title = title,
         x = "Position in the sequence",
         y = y_label) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  return(HaDeXify(state_comp_plot))
  
}


