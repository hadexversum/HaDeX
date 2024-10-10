#' Volcano plot
#'
#' @description Volcano plot for differential deuterim uptake
#' between two biological states
#' 
#' @importFrom ggplot2 coord_cartesian
#'
#' @param p_dat data produced by the \code{\link{create_p_diff_uptake_dataset}}
#' function
#' @param state_1 selected biological state for given protein
#' @param state_2 selected biological state for given protein
#' @param adjust_axes \code{\link{logical}}, indicator if the X-axis 
#' is symmetrical in relation to 0
#' @param show_confidence_limits \code{\link{logical}}, indicates if the hybrid 
#' test confidence levels are shown
#' @param confidence_level confidence level for the test, from range [0, 1]. It
#' should be the same as used to prepare p_dat
#' @param color_times \code{\link{logical}}, indicator if different time points 
#' are distinguishable by color
#' @param show_insignificant_grey \code{\link{logical}}, indicator if the 
#' values not passing the test are shown in grey
#' @param hide_insignificant \code{\link{logical}}, indicator if the 
#' values not passing the test are hidden
#' @param theoretical \code{logical}, indicator if values are 
#' calculated using theoretical controls
#' @param fractional \code{logical}, indicator if values are shown 
#' in fractional form 
#' @inheritParams plot_butterfly
#'
#' @details The function \code{\link{plot_volcano}} generates the 
#' volcano plot based on supplied p_dat. 
#' On X-axis there is differential deuterium uptake in selected form.
#' On Y-axis there is the P-value from t-Student test between two
#' biological states. Based on selected confidence level, the confidence
#' limits are calculated to indicate statistically significant values -
#' shown as red dotted lines. The values that are in upper left and right
#' corners pass the hybrid test.
#'
#' @return a \code{\link{ggplot}} object
#' 
#' @references Hageman, T. S. & Weis, D. D. Reliable Identification of Significant
#' Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements
#' Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
#' @references Houde, D., Berkowitz, S.A., and Engen, J.R. (2011).
#' The Utility of Hydrogen/Deuterium Exchange Mass Spectrometry in
#' Biopharmaceutical Comparability Studies. J Pharm Sci 100, 2071–2086.
#'
#' @seealso
#' \code{\link{create_p_diff_uptake_dataset}}
#'
#' @examples
#' dat <-  read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' p_dat <- create_p_diff_uptake_dataset(dat)
#' plot_volcano(p_dat, show_confidence_limits = T)
#' 
#' plot_volcano(p_dat, show_confidence_limits = T, show_insignificant_grey = T)
#' plot_volcano(p_dat, show_confidence_limits = T, hide_insignificant = T)
#' plot_volcano(p_dat, show_confidence_limits = T, hide_insignificant = T, show_insignificant_grey = T)
#' 
#' @export plot_volcano

plot_volcano <- function(p_dat,
                         state_1 = "",
                         state_2 = "",
                         adjust_axes = TRUE,
                         show_confidence_limits = FALSE,
                         confidence_level = 0.98,
                         color_times = TRUE,
                         show_insignificant_grey = FALSE,
                         hide_insignificant = FALSE,
                         fractional = FALSE,
                         theoretical = FALSE,
                         interactive = getOption("hadex_use_interactive_plots")) {
  
  if(hide_insignificant & show_insignificant_grey){
    
    message("Chosen parameters are in conflict. The unsignifiant values are in grey.")
    hide_insignificant <- F
    
  }
  
  if (fractional){
    
    x_label <- "Mass difference [%]"
    
    if(theoretical){
      
      value <- "diff_theo_frac_deut_uptake"
      err_value <- "err_diff_theo_frac_deut_uptake"
      
      
    } else {
      
      value <- "diff_frac_deut_uptake"
      err_value <- "err_diff_frac_deut_uptake" 
    }
    
  } else {
    
    x_label <- "Mass difference [Da]"
    
    if(theoretical){
      
      value <- "diff_theo_deut_uptake"
      err_value <- "err_diff_theo_deut_uptake"
      
      
    } else {
      
      value <- "diff_deut_uptake"
      err_value <- "err_diff_deut_uptake"
      
    }
  }
  
  plot_dat <- data.table(ID = p_dat[["ID"]],
                         Exposure = as.factor(p_dat[["Exposure"]]),
                         value = p_dat[[value]],
                         err_value = p_dat[[err_value]],
                         Sequence = p_dat[["Sequence"]],
                         Start = p_dat[["Start"]],
                         End = p_dat[["End"]],
                         P_value = p_dat[["P_value"]],
                         log_p_value = p_dat[["log_p_value"]])
  
  y_threshold <- -log(1 - confidence_level)
  
  t_value <- qt(c((1 - confidence_level)/2, 1-(1 - confidence_level)/2), df = 2)[2]
  x_threshold <- t_value * mean(plot_dat[["err_value"]], na.rm = TRUE)/sqrt(length(plot_dat))
  
  plot_dat[, valid := abs(value) > x_threshold & log_p_value > y_threshold]
  
  if(hide_insignificant) {
    
    plot_dat <- filter(plot_dat, valid)
    
  }
  
  chosen_geom_point <- if (interactive) geom_point_interactive( 
    aes(tooltip = glue(
      "{Sequence}
       Position: {Start}-{End}
       Exposure: {Exposure} min
       Difference: {round(value, 2)}
       P value: {round(P_value, 4)}
       -log(P value): {round(log_p_value, 2)}"
    )),
    size = 2
  ) else geom_point(size = 2)

  volcano_plot <- ggplot(
    plot_dat, 
    aes(
      x = value, 
      y = log_p_value, 
      color = if (color_times)
        as.factor(Exposure)
      else NULL
    )) +
    geom_errorbar(aes(xmin = value - err_value, 
                      xmax = value + err_value), 
                  color = "grey77") +
    chosen_geom_point +
    labs(title = paste0("Volcano Plot ", state_1, " " , state_2),
         x = x_label,
         y = "-log(P value)")
  
  
  if (color_times) {
    volcano_plot <- volcano_plot +
      labs(color = "Exposure") 
  }
  
  if(adjust_axes){
    
    x_max <- ceiling(max(abs(plot_dat[["value"]])))
    y_max <- ceiling(max(plot_dat[["log_p_value"]])) + 2
    
    volcano_plot <- volcano_plot +
      coord_cartesian(xlim = c(-x_max, x_max), 
                      ylim = c(0, y_max), 
                      expand = FALSE)
    
  }
  
  if(show_confidence_limits){
    
    volcano_plot <- volcano_plot +
      geom_segment(aes(x = -x_threshold, xend = -x_threshold, 
                       y = y_threshold, yend = Inf), 
                   linetype = "dashed", color = "red") +
      geom_segment(aes(x = x_threshold, xend = x_threshold, 
                       y = y_threshold, yend = Inf), 
                   linetype = "dashed", color = "red") +
      geom_segment(aes(y = y_threshold, yend = y_threshold, 
                       x = -Inf, xend = -x_threshold), 
                   linetype = "dashed", color = "red") +
      geom_segment(aes(y = y_threshold, yend = y_threshold,
                       x = x_threshold, xend = Inf), 
                   linetype = "dashed", color = "red")
    
  }
  
  if(show_insignificant_grey){
    
    volcano_plot <- volcano_plot + 
      geom_point(data = subset(plot_dat, !valid), 
                 aes(x = value, y = log_p_value), 
                 color = "grey77")
    
  }
  
  return(HaDeXify(volcano_plot))
  
}
