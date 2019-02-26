#' comparison_plot
#' 
#' Produces comparison_plot based on previously processed data - theoretical or experimental. User can change labels if needed.
#' 
#' @importFrom ggplot2 ggplot
#' 
#' @param calc_dat processed data from DynamiX file - using prepare_dataset
#' @param x_lab x label chosen by user. If unused, default label is chosen
#' @param y_lab y label chosen by user. If unused, default label is chosen
#' @param plot_title title label chosen by user. If unused, default label is chosen
#' @param theoretical logical value to determine if plot is theoretical or not. default : false
#' 
#' @export comparison_plot

comparison_plot <- function(calc_dat,
                            x_lab = NA,
                            y_lab = NA,
                            plot_title = NA,
                            theoretical = FALSE){
  
  if (theoretical) {
    
    x_lab <- ifelse(is.na(x_lab), "Position in sequence", x_lab)
    y_lab <- ifelse(is.na(y_lab), "Theoretical fraction Exchanged", y_lab)
    plot_title <- ifelse(is.na(plot_title), "Theoretical fraction exchanged in state comparison in chosen time", plot_title)

    ggplot()+
      geom_segment(data = calc_dat, aes(x = Start, y = avg_theo_in_time_1, xend = End, yend = avg_theo_in_time_1, color = "state_1")) +
      geom_segment(data = calc_dat, aes(x = Start, y = avg_theo_in_time_2, xend = End, yend = avg_theo_in_time_2, color = "state_2")) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = avg_theo_in_time_1 - err_avg_theo_in_time_1, ymax = avg_theo_in_time_1 + err_avg_theo_in_time_1, color = "state_1")) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = avg_theo_in_time_2 - err_avg_theo_in_time_2, ymax = avg_theo_in_time_2 + err_avg_theo_in_time_2, color = "state_2")) +
      labs(x = x_lab, y = y_lab, title = plot_title) +
      theme(legend.position = "bottom") +
      scale_y_continuous(breaks = seq(0, 1.2, 0.2), expand = c(0, 0), limits = c(0, 1.2))
    
  } else {
    
    x_lab <- ifelse(is.na(x_lab), "Position in sequence", x_lab)
    y_lab <- ifelse(is.na(y_lab), "Fraction Exchanged", y_lab)
    plot_title <- ifelse(is.na(plot_title), "Fraction exchanged in state comparison in chosen time", plot_title)

    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = frac_exch_state_1, xend = End, yend = frac_exch_state_1, color = "state_1")) +
      geom_segment(data = calc_dat, aes(x = Start, y = frac_exch_state_2, xend = End, yend = frac_exch_state_2, color = "state_2")) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = frac_exch_state_1 - err_frac_exch_state_1, ymax = frac_exch_state_1 + err_frac_exch_state_1, color = "state_1")) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = frac_exch_state_2 - err_frac_exch_state_2, ymax = frac_exch_state_2 + err_frac_exch_state_2, color = "state_2")) +
      labs(x = x_lab, y = y_lab, title = plot_title) +
      theme(legend.position = "bottom") +
      scale_y_continuous(breaks = seq(0, 1.2, 0.2), expand = c(0, 0), limits = c(0, 1.2))
    
  }
  
  
}
