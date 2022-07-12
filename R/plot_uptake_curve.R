#' Deuterium uptake curve 
#' 
#' @description Plot deuterium uptake curve for selected peptides 
#' 
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_ribbon geom_line scale_y_continuous scale_x_log10
#' 
#' @param uc_dat data produced by \code{\link{calculate_kinetics}} 
#' or \code{\link{calculate_peptide_kinetics}} or \code{\link{create_kinetic_dataset}}
#' functions
#' @param theoretical \code{logical}, indicator if values are 
#' calculated using theoretical controls
#' @param fractional \code{logical}, indicator if values are shown 
#' in fractional form 
#' @param uncertainty_type type of uncertainty presentation, possible values:
#' "ribbon", "bars" or "bars + line"
#' @param log_x \code{logical}, indicator if the X axis values 
#' are transformed to log10
#' 
#' @details The function \code{\link{plot_uptake_curve}} generates
#' the deuterium uptake curve for selected peptides 
#' from selected protein.
#' On X-axis there are time points of measurements. On Y-axis there
#' is deuterium uptake in selected form. The combined and propagated
#' uncertainty can be presentes as ribbons or error bars.
#' 
#' @return a \code{\link{ggplot}} object
#'  
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_kinetics}}
#' \code{\link{calculate_peptide_kinetics}}
#' \code{\link{create_kinetic_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'
#' uc_dat <- calculate_kinetics(dat, protein = "db_CD160",
#'                              sequence = "INITSSASQEGTRLN", 
#'                              state = "CD160",
#'                              start = 1, end = 15,
#'                              time_0 = 0.001, time_100 = 1440)
#' plot_uptake_curve(uc_dat = uc_dat, 
#'                   theoretical = FALSE, 
#'                   fractional = TRUE)
#' 
#' uc_dat2 <- calculate_peptide_kinetics(dat, protein = "db_CD160",
#'                                       sequence = "INITSSASQEGTRLN", 
#'                                       states = c("CD160", "CD160_HVEM"),
#'                                       start = 1, end = 15,
#'                                       time_0 = 0.001, time_100 = 1440)
#' plot_uptake_curve(uc_dat = uc_dat2, 
#'                   theoretical = FALSE, 
#'                   fractional = TRUE)
#'                 
#' @export plot_uptake_curve

plot_uptake_curve <- function(uc_dat, 
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
  
  plot_dat <- data.frame(Sequence = uc_dat[["Sequence"]],
                         Start = uc_dat[["Start"]],
                         End = uc_dat[["End"]],
                         State = uc_dat[["State"]],
                         time_t = uc_dat[["time_chosen"]],
                         value = uc_dat[[value]],
                         err_value = uc_dat[[err_value]],
                         prop = paste0(uc_dat[["Sequence"]], "-", uc_dat[["State"]]))
  
  uc_plot <- ggplot(plot_dat, aes(x = time_t, y = value, group = prop)) +
    geom_point(aes(color = prop), size = 2) + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Time points [min]", 
         y = y_label,
         title = title)
  
  if(log_x){ err_width = 0.1 } else { err_width = 2 }
  
  if(uncertainty_type == "ribbon"){
    
    uc_plot <- uc_plot +
      geom_ribbon(aes(ymin = value - err_value, ymax = value + err_value, fill = prop), alpha = 0.15) +
      geom_line(aes(color = prop)) 
    
  } else if (uncertainty_type == "bars") {
    
    uc_plot <- uc_plot +
      geom_errorbar(aes(x = time_t, ymin = value - err_value, ymax = value + err_value, color = prop),
                    width = err_width)
    
  } else if (uncertainty_type == "bars + line"){
    
    uc_plot <- uc_plot +
      geom_errorbar(aes(x = time_t, ymin = value - err_value, ymax = value + err_value, color = prop),
                    width = err_width) + 
      geom_line(aes(color = prop))
    
  }
  
  if(log_x){
    
    uc_plot <- uc_plot + 
      scale_x_log10()
    
  }
  
  return(HaDeXify(uc_plot))
}

