#' woods_plot
#' 
#' Produces Woods' plot based on previously processed data - theoretical or experimental. User can change labels if needed.
#' 
#' @importFrom ggplot2 ggplot
#' 
#' @param calc_dat processed data from DynamiX file - using prepare_dataset
#' @param x_lab x label chosen by user. If unused, default label is chosen
#' @param y_lab y label chosen by user. If unused, default label is chosen
#' @param plot_title title label chosen by user. If unused, default label is chosen
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
#'            theoretical = TRUE)
#' woods_plot(calc_dat = calc_dat)
#' 
#' @export woods_plot

woods_plot <- function(calc_dat,
                       x_lab = NA,
                       y_lab = NA,
                       plot_title = NA,
                       theoretical = FALSE,
                       relative = TRUE){
  
  if (theoretical) {
    
    if (relative) {
      
      x_lab <- ifelse(is.na(x_lab), "Position in sequence", x_lab)
      y_lab <- ifelse(is.na(y_lab), TeX("$\\Delta$ Theoretical fraction Exchanged"), y_lab)
      plot_title <- ifelse(is.na(plot_title), expression(paste(Delta, " Theoretical fraction exchanged between states in chosen time")), plot_title)
      
      ggplot() +
        geom_segment(data = calc_dat, aes(x = Start, y = diff_theo_frac_exch, xend = End, yend = diff_theo_frac_exch)) +
        geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = diff_theo_frac_exch - err_diff_theo_frac_exch, ymax = diff_theo_frac_exch + err_diff_theo_frac_exch)) +
        scale_y_continuous(breaks = seq(-1, 1, 0.2), expand = c(0, 0), limits = c(-1, 1)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .5) +
        labs(x = x_lab, y = y_lab, title = plot_title)
      
    } else {
      
      x_lab <- ifelse(is.na(x_lab), "Position in sequence", x_lab)
      y_lab <- ifelse(is.na(y_lab), TeX("$\\Delta$ theoretical absolute value Exchanged"), y_lab)
      plot_title <- ifelse(is.na(plot_title), expression(paste(Delta, " Theoretical absoute value exchanged between states in chosen time")), plot_title)
      
      ggplot() +
        geom_segment(data = calc_dat, aes(x = Start, y = abs_diff_theo_frac_exch, xend = End, yend = abs_diff_theo_frac_exch)) +
        geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_diff_theo_frac_exch - err_abs_diff_theo_frac_exch, ymax = abs_diff_theo_frac_exch + err_abs_diff_theo_frac_exch)) +
        scale_y_continuous(expand = c(0, 0)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .5) +
        labs(x = x_lab, y = y_lab, title = plot_title)
      
    }
    
  } else {
    
    if (relative) {
      
      x_lab <- ifelse(is.na(x_lab), "Position in sequence", x_lab)
      y_lab <- ifelse(is.na(y_lab), TeX("$\\Delta$ Fraction Exchanged"), y_lab)
      plot_title <- ifelse(is.na(plot_title), expression(paste(Delta, " Fraction exchanged between states in chosen time")), plot_title)
      
      ggplot() +
        geom_segment(data = calc_dat, aes(x = Start, y = diff_frac_exch, xend = End, yend = diff_frac_exch)) +
        geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = diff_frac_exch - err_frac_exch, ymax = diff_frac_exch + err_frac_exch)) +
        scale_y_continuous(breaks = seq(-1, 1, 0.2), expand = c(0, 0), limits = c(-1, 1)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .5) +
        labs(x = x_lab, y = y_lab, title = plot_title)
      
    } else {
      
      x_lab <- ifelse(is.na(x_lab), "Position in sequence", x_lab)
      y_lab <- ifelse(is.na(y_lab), TeX("$\\Delta$ absolute value exchanged"), y_lab)
      plot_title <- ifelse(is.na(plot_title), expression(paste(Delta, " Absolute value exchanged between states in chosen time")), plot_title)
      
      ggplot() +
        geom_segment(data = calc_dat, aes(x = Start, y = abs_diff_frac_exch, xend = End, yend = abs_diff_frac_exch)) +
        geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_diff_frac_exch - err_abs_diff_frac_exch, ymax = abs_diff_frac_exch + err_abs_diff_frac_exch)) +
        scale_y_continuous(expand = c(0, 0)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "red", size = .5) +
        labs(x = x_lab, y = y_lab, title = plot_title)
      
    }
   
  
  }
  
}
