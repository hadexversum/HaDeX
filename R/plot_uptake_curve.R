#' Deuterium uptake curve 
#' 
#' @description Plot deuterium uptake curve for selected peptides 
#' 
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_ribbon geom_line scale_y_continuous scale_x_log10 guides
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
#' @inheritParams plot_butterfly
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
#'                                       
#'                                       
#' plot_uptake_curve(uc_dat = uc_dat2, 
#'                   theoretical = FALSE, 
#'                   fractional = TRUE)
#'                 
#' @export plot_uptake_curve

plot_uptake_curve <- function(uc_dat, 
                              theoretical = FALSE, 
                              fractional = FALSE,
                              uncertainty_type = "ribbon",
                              log_x = TRUE,
                              interactive = getOption("hadex_use_interactive_plots")){
  
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
  
  title <- paste0(title, " of ", uc_dat[["Sequence"]])
  
  plot_dat <- data.frame(Sequence = uc_dat[["Sequence"]],
                         Start = uc_dat[["Start"]],
                         End = uc_dat[["End"]],
                         State = uc_dat[["State"]],
                         time_t = uc_dat[["time_chosen"]],
                         value = uc_dat[[value]],
                         err_value = uc_dat[[err_value]]) 
                         # prop = uc_dat[["State"]]) ##paste0(uc_dat[["Sequence"]], "-", 
                                       
  
  chosen_geom_point <- if (interactive) geom_point_interactive( 
    aes(tooltip = glue(
      "{Sequence}
       State: {State}
       Position: {Start}-{End}
       Value: {round(value, 2)}
       Time point: {time_t} min"
    )),
    size = 2
  ) else geom_point(size = 2)
  
  err_width <- if (log_x) 0.1 else 2
  
  chosen_uncertainty_geom <- switch (
    uncertainty_type,
    ribbon = geom_ribbon(aes(color = NULL),  alpha = 0.15),
    bars = geom_errorbar(width = err_width),
    `bars + line` = geom_errorbar(width = err_width)
  )
  
  uc_plot <- ggplot(
    plot_dat, 
    aes(
      x = time_t, 
      y = value, 
      group = State, 
      color = State, 
      fill = State,
      ymin = value - err_value, 
      ymax = value + err_value
    )) +
    chosen_uncertainty_geom +
    chosen_geom_point + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Time points [min]", 
         y = y_label,
         title = title) +
    guides(fill = guide_legend(title=""),
           color = guide_legend(title=""),
           group = guide_legend(title=""))
  
  if (uncertainty_type %in% c("ribbon", "bars + line"))
    uc_plot <- uc_plot + geom_line()
  
  if(log_x){
    
    uc_plot <- uc_plot + 
      scale_x_log10()
    
  }
  
  return(HaDeXify(uc_plot))
}

