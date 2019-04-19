#' comparison_plot
#' 
#' Produces comparison_plot based on previously processed data - theoretical or experimental. User can change labels if needed.
#' 
#' @importFrom ggplot2 ggplot geom_segment aes geom_errorbar labs theme scale_y_continuous geom_hline element_blank
#' @importFrom latex2exp TeX
#' 
#' @param calc_dat processed data from DynamiX file - using prepare_dataset
#' @param theoretical logical value to determine if plot is theoretical or not. default : false
#' @param relative logical value to determine if values are relative or absolute. default : true
#' @param state_first first state name
#' @param state_second second state name 
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
#' comparison_plot(calc_dat = calc_dat,
#'                 theoretical = TRUE,
#'                 relative = TRUE,
#'                 state_first = "CD160",
#'                 state_second = "CD160_HVEM")
#' comparison_plot(calc_dat = calc_dat,
#'                 theoretical = FALSE,
#'                 relative = TRUE,
#'                 state_first = "CD160",
#'                 state_second = "CD160_HVEM")  
#' comparison_plot(calc_dat = calc_dat,
#'                 theoretical = TRUE,
#'                 relative = FALSE,
#'                 state_first = "CD160",
#'                 state_second = "CD160_HVEM")
#' comparison_plot(calc_dat = calc_dat,
#'                 theoretical = FALSE,
#'                 relative = FALSE,
#'                 state_first = "CD160",
#'                 state_second = "CD160_HVEM")                
#' 
#' @export comparison_plot

comparison_plot <- function(calc_dat,
                            theoretical = FALSE,
                            relative = TRUE,
                            state_first = "state_first",
                            state_second = "state_second"){
  
  if (relative) {
    
    relative_comparison_plot(calc_dat,
                             theoretical = theoretical,
                             state_first = state_first,
                             state_second = state_second)
    
  } else {
    
    absolute_comparison_plot(calc_dat,
                             theoretical = theoretical,
                             state_first = state_first,
                             state_second = state_second)
    
  }
  
}


absolute_comparison_plot <- function(calc_dat,
                                     theoretical,
                                     state_first,
                                     state_second){
  if (theoretical) {
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = abs_avg_theo_in_time_1, xend = End, yend = abs_avg_theo_in_time_1, color = state_first)) +
      geom_segment(data = calc_dat, aes(x = Start, y = abs_avg_theo_in_time_2, xend = End, yend = abs_avg_theo_in_time_2, color = state_second)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_avg_theo_in_time_1 - err_abs_avg_theo_in_time_1, ymax = abs_avg_theo_in_time_1 + err_abs_avg_theo_in_time_1, color = state_first)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_avg_theo_in_time_2 - err_abs_avg_theo_in_time_2, ymax = abs_avg_theo_in_time_2 + err_abs_avg_theo_in_time_2, color = state_second)) +
      labs(x = "Position in sequence", 
           y = "Theoretical absolute value exchanged [Da]", 
           title = "Theoretical absolute value exachanged in state comparison in chosen time") +
      theme_bw() + 
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      scale_y_continuous(expand = c(0, 0)) 
    
  } else {
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = abs_frac_exch_state_1, xend = End, yend = abs_frac_exch_state_1, color = state_first)) +
      geom_segment(data = calc_dat, aes(x = Start, y = abs_frac_exch_state_2, xend = End, yend = abs_frac_exch_state_2, color = state_second)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_frac_exch_state_1 - err_abs_frac_exch_state_1, ymax = abs_frac_exch_state_1 + err_abs_frac_exch_state_1, color = state_first)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_frac_exch_state_2 - err_abs_frac_exch_state_2, ymax = abs_frac_exch_state_2 + err_abs_frac_exch_state_2, color = state_second)) +
      labs(x = "Position in sequence", 
           y = "Absolute value exchanged [Da]", 
           title = "Absolute value exchanged in state comparison in chosen time") +
      theme_bw() + 
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      scale_y_continuous(expand = c(0, 0))
    
  }
  
}

relative_comparison_plot <- function(calc_dat,
                                     theoretical,
                                     state_first,
                                     state_second){
  if (theoretical) {
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = avg_theo_in_time_1, xend = End, yend = avg_theo_in_time_1, color = state_first)) +
      geom_segment(data = calc_dat, aes(x = Start, y = avg_theo_in_time_2, xend = End, yend = avg_theo_in_time_2, color = state_second)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = avg_theo_in_time_1 - err_avg_theo_in_time_1, ymax = avg_theo_in_time_1 + err_avg_theo_in_time_1, color = state_first)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = avg_theo_in_time_2 - err_avg_theo_in_time_2, ymax = avg_theo_in_time_2 + err_avg_theo_in_time_2, color = state_second)) +
      labs(x = "Position in sequence", 
           y = "Theoretical fraction exchanged [%]", 
           title = "Theoretical fraction exchanged in state comparison in chosen time") +
      theme_bw() + 
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      scale_y_continuous(breaks = seq(0, 1.2, 0.2), expand = c(0, 0), limits = c(0, 1.2))
    
  } else {
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = frac_exch_state_1, xend = End, yend = frac_exch_state_1, color = state_first)) +
      geom_segment(data = calc_dat, aes(x = Start, y = frac_exch_state_2, xend = End, yend = frac_exch_state_2, color = state_second)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = frac_exch_state_1 - err_frac_exch_state_1, ymax = frac_exch_state_1 + err_frac_exch_state_1, color = state_first)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = frac_exch_state_2 - err_frac_exch_state_2, ymax = frac_exch_state_2 + err_frac_exch_state_2, color = state_second)) +
      labs(x = "Position in sequence", 
           y = "Fraction exchanged [%]", 
           title = "Fraction exchanged in state comparison in chosen time") +
      theme_bw() + 
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      scale_y_continuous(breaks = seq(0, 1.2, 0.2), expand = c(0, 0), limits = c(0, 1.2))
    
  }
  
}

