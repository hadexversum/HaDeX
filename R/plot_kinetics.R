#' Plot kinetics data
#' 
#' @description Plots kinetics of the hydrogen-deuterium exchange for specific peptides. 
#' 
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_ribbon geom_line scale_y_continuous
#' 
#' @param kin_dat calculated kinetic data by \code{\link{calculate_kinetics}} 
#' function
#' @param theoretical \code{logical}, determines if plot shows theoretical values
#' @param relative \code{logical}, determines if values are relative or absolute
#' 
#' @seealso \code{\link{calculate_kinetics}}
#' 
#' @details This function visualises the output of the  
#' \code{\link{calculate_kinetics}} function. 
#' Based on supplied parameters appopriate columns are chosen for the plot. 
#' The uncertainty associated with each peptide is shown as a ribbon. 
#' Axis are labeled according to the supplied parameters but no title is provied.
#' 
#' If you want to plot data for more then one peptide in one state, join 
#' calculated data by using \code{\link{bind_rows}} from dplyr package and 
#' pass the result as kin_dat.
#' 
#' @return a \code{\link[ggplot2]{ggplot}} object.
#' 
#' @examples 
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'
#' # calculate data for the sequence INITSSASQEGTRLN in the CD160 state 
#' (kin1 <- calculate_kinetics(dat, 
#'                            protein = "db_CD160",
#'                            sequence = "INITSSASQEGTRLN", 
#'                            state = "CD160",
#'                            start = 1, 
#'                            end = 15,
#'                            time_in = 0.001, 
#'                            time_out = 1440))
#'
#' # calculate data for the sequence INITSSASQEGTRLN in the CD160_HVEM state 
#' (kin2 <- calculate_kinetics(dat, 
#'                            protein = "db_CD160",
#'                            sequence = "INITSSASQEGTRLN", 
#'                            state = "CD160_HVEM",
#'                            start = 1, 
#'                            end = 15,
#'                            time_in = 0.001, 
#'                            time_out = 1440)) 
#'                          
#' # load extra packages 
#' library(dplyr)
#'   
#' # plot a single peptide - theoretical and relative
#' plot_kinetics(kin_dat = kin1, 
#'               theoretical = TRUE, 
#'               relative = TRUE)
#'                 
#' # plot joined data - experimental and absolute
#' bind_rows(kin1, kin2) %>%
#'   plot_kinetics(theoretical = FALSE, 
#'                 relative = FALSE)
#' @export plot_kinetics
plot_kinetics <- function(kin_dat, 
                          theoretical = FALSE, 
                          relative = TRUE){
  
  if (theoretical){
    
    if (relative){
      
      kin_dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = avg_theo_in_time, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = avg_theo_in_time - err_avg_theo_in_time, ymax = avg_theo_in_time + err_avg_theo_in_time, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(limits = c(-.1, 1.1)) + 
        labs(x = "Time points [min]", 
             y = "Theoretical deuteration [%]")
      
      } else {
      
      kin_dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = abs_avg_theo_in_time, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = abs_avg_theo_in_time - err_abs_avg_theo_in_time, ymax = abs_avg_theo_in_time + err_abs_avg_theo_in_time, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(limits = c(0, NA)) + 
        labs(x = "Time points [min]", 
             y = "Theoretical deuteration [Da]")
      
    }
    
  } else {
    
    if (relative){
      
      kin_dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = frac_exch_state, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = frac_exch_state - err_frac_exch_state, ymax = frac_exch_state + err_frac_exch_state, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(limits = c(-.1, 1.1)) + 
        labs(x = "Time points [min]", 
             y = "Deuteration [%]")
      
    } else {
      
      kin_dat %>% 
        mutate(prop = paste0(Sequence, "-", State)) %>%
        ggplot(aes(x = time_chosen, y = abs_frac_exch_state, group = prop)) +
        geom_point() + 
        geom_ribbon(aes(ymin = abs_frac_exch_state - err_abs_frac_exch_state, ymax = abs_frac_exch_state + err_abs_frac_exch_state, fill = prop), alpha = 0.15) +
        geom_line(aes(color = prop)) +
        theme(legend.position = "bottom",
              legend.title = element_blank()) +
        scale_y_continuous(limits = c(0, NA)) + 
        labs(x = "Time points [min]", 
             y = "Deuteration [Da]")
      
    }
    
  }
  
}