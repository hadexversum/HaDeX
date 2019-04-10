#' woods_plot
#' 
#' Produces Woods' plot based on previously processed data - theoretical or experimental. User can change labels if needed.
#' 
#' @importFrom ggplot2 ggplot
#' 
#' @param calc_dat processed data from DynamiX file - using prepare_dataset
#' @param theoretical logical value to determine if plot is theoretical or not. default : false
#' @param relative logical value to determine if values are relative or absolute. default : true
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' calc_dat <- prepare_dataset(dat,
#'                             in_state_first = "CD160_0.001",
#'                             chosen_state_first = "CD160_1",
#'                             out_state_first = "CD160_1440",
#'                             in_state_second = "CD160_HVEM_0.001",
#'                             chosen_state_second = "CD160_HVEM_1",
#'                             out_state_second = "CD160_HVEM_1440") 
#' woods_plot(calc_dat = calc_dat,
#'            theoretical = TRUE,
#'            relative = TRUE)
#' woods_plot(calc_dat = calc_dat,
#'            theoretical = FALSE,
#'            relative = TRUE)
#' woods_plot(calc_dat = calc_dat,
#'            theoretical = TRUE,
#'            relative = FALSE)
#' woods_plot(calc_dat = calc_dat,
#'            theoretical = FALSE,
#'            relative = FALSE)
#'             
#' @export woods_plot

woods_plot <- function(calc_dat,
                       theoretical = FALSE,
                       relative = TRUE){
  if (relative) {
    
    relative_woods_plot(calc_dat = calc_dat,
                        theoretical = theoretical)
    
  } else {
    
    absolute_woods_plot(calc_dat = calc_dat,
                        theoretical = theoretical)
    
  }
  
}

absolute_woods_plot <- function(calc_dat,
                                theoretical = FALSE){
  
  if (theoretical){
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = abs_diff_theo_frac_exch, xend = End, yend = abs_diff_theo_frac_exch)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_diff_theo_frac_exch - err_abs_diff_theo_frac_exch, ymax = abs_diff_theo_frac_exch + err_abs_diff_theo_frac_exch)) +
      scale_y_continuous(expand = c(0, 0)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .7) +
      labs(x = "Position in sequence", 
           y = expression(paste(Delta, " theoretical absolute value exchanged [Da]")), 
           title = expression(paste(Delta, " Theoretical absoute value exchanged between states in chosen time")))
    
  } else {
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = abs_diff_frac_exch, xend = End, yend = abs_diff_frac_exch)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_diff_frac_exch - err_abs_diff_frac_exch, ymax = abs_diff_frac_exch + err_abs_diff_frac_exch)) +
      scale_y_continuous(expand = c(0, 0)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .7) +
      labs(x = "Position in sequence", 
           y = expression(paste(Delta, " absolute value exchanged [Da]")), 
           title = expression(paste(Delta, " Absolute value exchanged between states in chosen time")))
    
  }
  
}

relative_woods_plot <- function(calc_dat,
                                theoretical = FALSE){
  
  if (theoretical){
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = diff_theo_frac_exch, xend = End, yend = diff_theo_frac_exch)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = diff_theo_frac_exch - err_diff_theo_frac_exch, ymax = diff_theo_frac_exch + err_diff_theo_frac_exch)) +
      scale_y_continuous(breaks = seq(-1, 1, 0.2), expand = c(0, 0), limits = c(-1, 1)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .7) +
      labs(x = "Position in sequence", 
           y = expression(paste(Delta, " theoretical fraction exchanged [%]")), 
           title = expression(paste(Delta, " Theoretical fraction exchanged between states in chosen time")))
    
  } else {
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = diff_frac_exch, xend = End, yend = diff_frac_exch)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = diff_frac_exch - err_frac_exch, ymax = diff_frac_exch + err_frac_exch)) +
      scale_y_continuous(breaks = seq(-1, 1, 0.2), expand = c(0, 0), limits = c(-1, 1)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .7) +
      labs(x = "Position in sequence",
           y = expression(paste(Delta, " fraction exchanged [%]")), 
           title = expression(paste(Delta, " Fraction exchanged between states in chosen time")))
    
  }
  
}
