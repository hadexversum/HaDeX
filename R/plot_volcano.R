#' Volcano plot
#'
#' @importFrom ggplot2 coord_cartesian
#'
#' @param p_dat data produced by the \code{\link{create_p_diff_uptake_dataset}}
#' function.
#' @param state_1 biological state for chosen protein. It is used in the title.
#' @param state_2 biological state for chosen protein. It is used in the title.
#' @param adjust_axes logical, indicating if the X-axis is symmetrical in
#' relation to 0.
#' @param show_confidence_limits logical, indicates if the hybrid testing
#' confidence intervals are shown.
#' @param confidence_level confidence level for the test, from range [0, 1]. It
#' should be the same as used in \code{\link{create_volcano_dataset}} function.
#' @param color_times logical, indicating if different time points on the plot
#' are distinguishable by color. 
#' @param relative ...
#' @param theoretical ...
#'
#' @details The data produced by \code{\link{create_volcano_dataset}} are plotted
#' in the form of a volcano plot. The generation of the data is described in the documentation
#' of \code{\link{create_volcano_dataset}} function. The confidence limit on
#' P-value is calculated based on the confidence level. The confidence limit on deuterium
#' uptake difference is calculated using the Houde test for the time point of measurement
#' from the provided data. The confidence limits are indicated by the red dotted
#' lines. The points above confidence limits (upper right and left corner) are
#' statistically significant in hybrid testing.
#' This plot is visible in GUI.
#'
#' @references Hageman, T. S. & Weis, D. D. Reliable Identification of Significant
#' Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements
#' Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
#' @references Houde, D., Berkowitz, S.A., and Engen, J.R. (2011).
#' The Utility of Hydrogen/Deuterium Exchange Mass Spectrometry in
#' Biopharmaceutical Comparability Studies. J Pharm Sci 100, 2071–2086.
#'
#' @return a \code{\link{ggplot}} object.
#'
#' @seealso
#' \code{\link{create_p_diff_uptake_dataset}}
#' \code{\link{show_volcano_data}}
#'
#' @examples
#' dat <-  read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' p_dat <- create_p_diff_uptake_dataset(dat)
#' plot_volcano(p_dat, show_confidence_limits = T)
#'
#' @export plot_volcano

plot_volcano <- function(p_dat,
                         state_1 = "",
                         state_2 = "",
                         adjust_axes = TRUE,
                         show_confidence_limits = FALSE,
                         confidence_level = 0.98,
                         color_times = TRUE,
                         fractional = F,
                         theoretical = F) {
  
  
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
  
  plot_dat <- data.frame(ID = p_dat[["ID"]],
                         Exposure = as.factor(p_dat[["Exposure"]]),
                         value = p_dat[[value]],
                         err_value = p_dat[[err_value]],
                         Sequence = p_dat[["Sequence"]],
                         Start = p_dat[["Start"]],
                         End = p_dat[["End"]],
                         P_value = p_dat[["P_value"]],
                         log_p_value = p_dat[["log_p_value"]])
  
  
  if (color_times){
    
    volcano_plot <- ggplot(plot_dat, aes(x = value, y = log_p_value)) +
      geom_point(aes(color = as.factor(Exposure))) +
      geom_errorbar(aes(xmin = value - err_value, xmax = value + err_value), alpha = 0.2) +
      labs(title = paste0("Volcano Plot ", state_1, " " , state_2),
           x = x_label,
           y = "-log(P value)") +
      labs(color = "Exposure") 
    
  } else {
    
    volcano_plot <- ggplot(plot_dat, aes(x = value, y = log_p_value)) +
      geom_point() +
      geom_errorbar(aes(xmin = value - err_value, xmax = value + err_value), alpha = 0.2) +
      labs(title = paste0("Volcano Plot ", state_1, " " , state_2),
           x = x_label,
           y = "-log(P value)")
  }
  
  if(adjust_axes){
    
    x_max <- ceiling(max(abs(plot_dat[["value"]])))
    y_max <- ceiling(max(plot_dat[["log_p_value"]])) + 2
    
    volcano_plot <- volcano_plot +
      coord_cartesian(xlim = c(-x_max, x_max), ylim = c(0, y_max), expand = FALSE)
    
  }
  
  if(show_confidence_limits){
    
    y_threshold <- -log(1 - confidence_level)
    
    t_value <- qt(c((1 - confidence_level)/2, 1-(1 - confidence_level)/2), df = 2)[2]
    x_threshold <- t_value * mean(plot_dat[["err_value"]], na.rm = TRUE)/sqrt(length(plot_dat))
    
    volcano_plot <- volcano_plot +
      geom_segment(aes(x = -x_threshold, xend = -x_threshold, y = y_threshold, yend = Inf), linetype = "dashed", color = "red") +
      geom_segment(aes(x = x_threshold, xend = x_threshold, y = y_threshold, yend = Inf), linetype = "dashed", color = "red") +
      geom_segment(aes(y = y_threshold, yend = y_threshold, x = -Inf, xend = -x_threshold), linetype = "dashed", color = "red") +
      geom_segment(aes(y = y_threshold, yend = y_threshold, x = x_threshold, xend = Inf), linetype = "dashed", color = "red")
    
  }
  
  return(HaDeXify(volcano_plot))
  
}
