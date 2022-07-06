#' Plot kinetics data
#' 
#' @description Plots kinetics of the hydrogen-deuterium exchange for specific peptides. 
#' 
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_ribbon geom_line scale_y_continuous scale_x_log10
#' 
#' @param kin_dat calculated kinetic data by \code{\link{calculate_kinetics}} 
#' or \code{\link{calculate_peptide_kinetics}} or \code{\link{create_kinetic_dataset}}
#' function.
#' @param theoretical \code{logical}, determines if plot shows theoretical values.
#' @param fractional \code{logical}, determines if plot shows fractional values.
#' @param uncertainty_type type of presenting uncertainty, possible values:
#' "ribbon", "bars" or "bars + line".
#' @param log_x \code{logical}, determines if x axis shows logarithmic values.
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
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_kinetics}}
#' \code{\link{calculate_peptide_kinetics}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'
#' # one peptide in one state
#' uc_dat <- calculate_kinetics(dat, 
#'                              protein = "db_CD160",
#'                              sequence = "INITSSASQEGTRLN", 
#'                              state = "CD160",
#'                              start = 1, 
#'                              end = 15,
#'                              time_0 = 0.001, 
#'                              time_100 = 1440)
#' plot_uptake_curve(uc_dat = uc_dat, 
#'                   theoretical = FALSE, 
#'                   fractional = TRUE)
#' 
#' # one peptide in all states         
#' uc_dat2 <- calculate_peptide_kinetics(dat, 
#'                                       protein = "db_CD160",
#'                                       sequence = "INITSSASQEGTRLN", 
#'                                       states = c("CD160", "CD160_HVEM"),
#'                                       start = 1, 
#'                                       end = 15,
#'                                       time_0 = 0.001, 
#'                                       time_100 = 1440)
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

