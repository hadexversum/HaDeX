#' Butterfly differential plot
#' 
#' @param diff_uptake_dat data produced by 
#' \code{\link{create_diff_uptake_dataset}} function.
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param uncertainty_type type of presenting uncertainty, possible values: 
#' "ribbon", "bars" or "bars + line".
#' @param show_confidence_limit \code{logical}, determines if confidence limits
#' are visible on the plot. 
#' @param confidence_level confidence level for confidence limit, if chosen
#' show_confidence_limit.
#' 
#' @details Function \code{\link{plot_butterfly_differential}} generates 
#' butterfly differential plot based on provided data and parameters. On X-axis 
#' there is peptide ID. On the Y-axis there is deuterium uptake difference in 
#' chosen form. Data from multiple time points of measurement is presented.
#' If chosen, there are confidence limits based on Houde test on provided 
#' confidence level.
#' This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot}} object. 
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{create_diff_uptake_dataset}}
#' 
#' @references Houde, D., Berkowitz, S.A., and Engen, J.R. (2011). 
#' The Utility of Hydrogen/Deuterium Exchange Mass Spectrometry in 
#' Biopharmaceutical Comparability Studies. J Pharm Sci 100, 2071–2086.
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' plot_butterfly_differential(diff_uptake_dat)
#' 
#' @export plot_butterfly_differential

plot_butterfly_differential <- function(diff_uptake_dat, 
                                        theoretical = FALSE, 
                                        fractional = FALSE,
                                        uncertainty_type = "ribbon",
                                        show_confidence_limit = FALSE,
                                        confidence_level = 0.98){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars", "bars + line"))
  
  if (theoretical) {
    
    title <- "Theoretical butterfly differential plot"
    
    if (fractional) {
      
      # theoretical & fractional
      value <- "diff_theo_frac_deut_uptake"
      err_value <- "err_diff_theo_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake difference [%]"
      
    } else {
      
      # theoretical & absolute
      value <- "diff_theo_deut_uptake"
      err_value <- "err_diff_theo_deut_uptake"
      y_label <- "Deuterium uptake difference [Da]"
      
    }
    
  } else {
    
    title <- "Butterfly differential plot"
    
    if (fractional) {
      
      # experimental & fractional
      value <- "diff_frac_deut_uptake"
      err_value <- "err_diff_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake difference [%]"
      
    } else {
      
      # experimental & absolute
      value <- "diff_deut_uptake"
      err_value <- "err_diff_deut_uptake"
      y_label <- "Deuterium uptake difference [Da]"
      
    }
    
  }
  
  plot_dat <- data.frame(ID = diff_uptake_dat[["ID"]],
                         Exposure = as.factor(diff_uptake_dat[["Exposure"]]),
                         value = diff_uptake_dat[[value]],
                         err_value = diff_uptake_dat[[err_value]],
                         Sequence = diff_uptake_dat[["Sequence"]],
                         Start = diff_uptake_dat[["Start"]],
                         End = diff_uptake_dat[["End"]])
  
  butterfly_differential_plot <- ggplot(plot_dat, aes(x = ID, y = value, color = Exposure)) +
    geom_point(aes(group = Exposure, color = Exposure)) +
    coord_cartesian(ylim = c(-.5, 1)) +
    labs(title = title,
         x = "Peptide ID",
         y = y_label) +
    theme(legend.position = "bottom")
  
  if(uncertainty_type == "ribbon"){
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_ribbon(aes(x = ID, ymin = value - err_value, ymax = value + err_value, fill = Exposure), alpha = 0.5, size = 0, linetype = "blank")
    
  } else if (uncertainty_type == "bars"){
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_errorbar(aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5)
    
  } else if (uncertainty_type == "bars + line"){
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_errorbar(aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5) +
      geom_line()
  }
  
  if(show_confidence_limit){
    
    t_value <- qt(c((1 - confidence_level)/2, 1-(1 - confidence_level)/2), df = 2)[2]
    x_threshold <- t_value * mean(plot_dat[["err_value"]], na.rm = TRUE)/sqrt(length(plot_dat))  
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_hline(aes(yintercept = x_threshold), linetype = "dashed", color = "black", size = .7) + 
      geom_hline(aes(yintercept = -x_threshold), linetype = "dashed", color = "black", size = .7) 
    
  }
  
  return(butterfly_differential_plot)
  
}
