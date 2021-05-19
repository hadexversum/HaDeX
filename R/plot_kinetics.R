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
#' @param fractional \code{logical}, determines if plot shows fractional values
#' @param uncertainty_type type of presenting uncertainty, possible values:
#' "ribbon", "bars" or "bars + line".
#' @param log_x \code{logical}, determines if x axis shows logarithmic values.
#' 
#' @seealso \code{\link{calculate_kinetics}}
#' 
#' @details This function visualizes the output of the  
#' \code{\link{calculate_kinetics}} function. 
#' Based on supplied parameters appropriate columns are chosen for the plot. 
#' The uncertainty associated with each peptide is shown as a ribbon. 
#' Axis are labeled according to the supplied parameters but no title is provided.
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
#' # plot a single peptide - theoretical and fractional
#' plot_kinetics(kin_dat = kin1, 
#'               theoretical = TRUE, 
#'               fractional = TRUE)
#'                 
#' # plot joined data - experimental and absolute
#' bind_rows(kin1, kin2) %>%
#'   plot_kinetics(theoretical = FALSE, 
#'                 fractional = FALSE)
#'                 
#' @export plot_kinetics
#' 
plot_kinetics <- function(kin_dat, 
                          theoretical = FALSE, 
                          fractional = FALSE,
                          uncertainty_type = "ribbon",
                          log_x = TRUE){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars", "bars + line"))
  
  if (theoretical){
    
    title <- "Theoretical uptake curve"
    
    if (fractional){
      
      value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake [%]"

      } else {
      
        value <- "theo_deut_uptake"
        err_value <- "err_theo_deut_uptake"
        y_label <- "Deuterium uptake [Da]"
      
    }
    
  } else {
    
    title <- "Uptake curve"
    
    if (fractional){
      
      value <- "frac_deut_uptake"
      err_value <- "err_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake [%]"
      
    } else {
      
      value <- "deut_uptake"
      err_value <- "err_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      
    }
    
  }
  
  plot_dat <- data.frame(Sequence = kin_dat[["Sequence"]],
                         Start = kin_dat[["Start"]],
                         End = kin_dat[["End"]],
                         State = kin_dat[["State"]],
                         time_chosen = kin_dat[["time_chosen"]],
                         value = kin_dat[[value]],
                         err_value = kin_dat[[err_value]],
                         prop = paste0(kin_dat[["Sequence"]], "-", kin_dat[["State"]]))
  
  kin_plot <- plot_dat %>% 
    ggplot(aes(x = time_chosen, y = value, group = prop)) +
    geom_point(aes(color = prop)) + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    scale_y_continuous(limits = c(0, NA)) + 
    labs(x = "Time points [min]", 
         y = y_label,
         title = title)
  
  if(uncertainty_type == "ribbon"){
    
    kin_plot <- kin_plot +
      geom_ribbon(aes(ymin = value - err_value, ymax = value + err_value, fill = prop), alpha = 0.15) +
      geom_line(aes(color = prop)) 
    
  } else if (uncertainty_type == "bars") {
    
    kin_plot <- kin_plot +
      geom_errorbar(aes(x = time_chosen, ymin = value - err_value, ymax = value + err_value, color = prop))
    
  } else if (uncertainty_type == "bars + line"){
    
    kin_plot <- kin_plot +
      geom_errorbar(aes(x = time_chosen, ymin = value - err_value, ymax = value + err_value, color = prop)) + 
      geom_line(aes(color = prop))
    
  }
  
  if(log_x){
    
    kin_plot <- kin_plot + 
      scale_x_log10()
    
  }
  
  kin_plot
}