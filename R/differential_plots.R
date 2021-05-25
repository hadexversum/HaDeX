#' Differential plot
#' 
#' @param dat produced by \code{\link{generate_differential_data_set}} function
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param confidence_limit ...
#' @param confidence_limit_2 ...
#' 
#' @details Function \code{\link{generate_differential_plot}} presents
#' provided data in a form of differential (Woods) plot. The plot show 
#' difference in exchange for two biological states, selected in 
#' generation of dataset at one time point of measurement .On X-axis 
#' there is a position in a sequence, with length of a segment of each 
#' peptide representing its length. On Y-axis there 
#' is deuterium uptake difference in chosen form. Error bars represents 
#' the combined and propagated uncertainty. 
#' This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{generate_differential_data_set}}
#' \code{\link{generate_differential_data}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- generate_differential_data_set(dat)
#' plot_differential(diff_uptake_dat)
#' 
#' @export plot_differential

plot_differential <- function(dat, 
                              theoretical = FALSE, 
                              fractional = FALSE,
                              confidence_limit = 0.98, 
                              confidence_limit_2 = 0.99){ 
  
  interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                confidence_limit = confidence_limit,
                                                theoretical = theoretical,
                                                fractional = fractional)
  
  interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                  confidence_limit = confidence_limit_2,
                                                  theoretical = theoretical,
                                                  fractional = fractional)
  
  if(theoretical){
    
    title <- "Theoretical differential plot"
    
    if(fractional){
      
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
    
    title <- "Differential plot"
    
    if(fractional){
      
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
  
  plot_dat <- data.frame(Protein = dat[["Protein"]],
                         Sequence = dat[["Sequence"]],
                         Start = dat[["Start"]],
                         End = dat[["End"]],
                         Med_Sequence = dat[["Med_Sequence"]],
                         value = dat[[value]],
                         err_value = dat[[err_value]])
  
  mutate(plot_dat, colour = case_when(
    value < interval_2[1] ~ "deepskyblue3",
    value < interval[1] ~ "deepskyblue1",
    value > interval_2[2] ~ "firebrick3",
    value > interval[2] ~ "firebrick1",
    TRUE ~ "azure3")) %>%
    ggplot() +
    geom_segment(aes(x = Start, y = value, xend = End, yend = value, color = colour)) +
    geom_errorbar(aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = colour)) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
    ## intervals
    geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
    geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
    geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
    geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
    scale_linetype_manual(values = c("dashed", "dotdash")) + 
    ## other
    scale_colour_identity() +
    labs(title = title,
         x_label = "Position in the sequence",
         y_label = y_label) + 
    theme(legend.title = element_blank(),
          legend.position = "bottom",
          legend.direction = "vertical") 
  
}

#' Differential butterfly plot
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
#' @details Function \code{\link{plot_differential_butterfly}} generates 
#' differential butterfly plot based on provided data and parameters. On X-axis 
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
#' plot_differential_butterfly(diff_uptake_dat)
#' 
#' @export plot_differential_butterfly

plot_differential_butterfly <- function(diff_uptake_dat, 
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

#' Differential chiclet plot
#' 
#' @param diff_uptake_dat data produced by 
#' \code{\link{create_diff_uptake_dataset}} function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param show_uncertainty \code{logical}, determines if the
#' uncertainty is shown. 
#' 
#' @details Function \code{\link{plot_differential_chiclet}} generates 
#' chiclet differential plot based on provided data and parameters.
#' On X-axis there is a peptide ID. On Y-axis are time points 
#' of measurement. Each tile for a peptide in time has a color value 
#' representing the deuterium uptake difference between chosen states, 
#' in a form based on provided criteria (e.q. fractional). Each tile has 
#' a plus sign, which size represent the uncertainty of measurement for 
#' chosen value.
#' This plot is visible in GUI.
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{create_diff_uptake_dataset}}
#' \code{\link{generate_chiclet_differential_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' plot_differential_chiclet(diff_uptake_dat)
#' 
#' @export plot_differential_chiclet

plot_differential_chiclet <- function(diff_uptake_dat, 
                                               theoretical = FALSE, 
                                               fractional = FALSE,
                                               show_uncertainty = FALSE){
  
  
  if (theoretical) {
    
    if (fractional) {
      
      # theoretical & fractional
      value <- "diff_theo_frac_deut_uptake"
      err_value <- "err_diff_theo_frac_deut_uptake"
      title <- "Theoretical chiclet differential plot"
      fill <- "Fractional DU Diff"
      
    } else {
      
      # theoretical & absolute
      value <- "diff_theo_deut_uptake"
      err_value <- "err_diff_theo_deut_uptake"
      title <- "Theoretical chiclet differential plot"
      fill <- "DU Diff"
      
    }
    
  } else {
    
    if (fractional) {
      
      # experimental & fractional
      value <- "diff_frac_deut_uptake"
      err_value <- "err_diff_frac_deut_uptake"
      title <- "Chiclet differential plot"
      fill <- "Fractional DU Diff"
      
    } else {
      
      # experimental & absolute
      value <- "diff_deut_uptake"
      err_value <- "err_diff_deut_uptake"
      title <- "Chiclet differential plot"
      fill <- "DU Diff"
      
    }
    
  }
  
  plot_dat <- data.frame(ID = diff_uptake_dat[["ID"]],
                         Exposure = as.factor(diff_uptake_dat[["Exposure"]]),
                         value = diff_uptake_dat[[value]],
                         err_value = diff_uptake_dat[[err_value]],
                         Sequence = diff_uptake_dat[["Sequence"]],
                         Start = diff_uptake_dat[["Start"]],
                         End = diff_uptake_dat[["End"]])
  
  chiclet_differential_plot <- ggplot(plot_dat, aes(y = Exposure, x = ID)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", guide = guide_legend(keywidth = 3)) +
    labs(title = title,
         y = "Exposure [min]",
         x = "Peptide ID",
         fill = fill) +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          legend.key = element_rect(colour = 'black', size = 1))
  
  if(show_uncertainty){
    
    chiclet_differential_plot <- chiclet_differential_plot +
      geom_point(aes(size = err_value), shape = 3) + 
      labs(size = "Err")
    
  } 
  
  return(chiclet_differential_plot)
  
}

#' Volcano plot
#' 
#' @importFrom ggplot2 coord_cartesian
#' 
#' @param vol_data data produced by the \code{\link{create_volcano_dataset}} 
#' function.
#' @param state_1 biological state for chosen protein. It is used in the title.
#' @param state_2 biological state for chosen protein. It is used in the title.
#' @param adjust_axes logical, indicating if the X-axis is symmetrical in 
#' relation to 0. 
#' @param show_confidence_limits logical, indicates if the hybrid testing 
#' confidence intervals are shown. 
#' @param confidence_level confidence level for the confidence intervals. It 
#' should be the same as used in \code{\link{generate_volcano_dataset}} function.
#' 
#' @details The data produced by \code{\link{generate_volcano_dataset}} are plotted
#' in the form of a volcano plot. The generation of the data is described in the documentation
#' of \code{\link{generate_volcano_dataset}} function. The confidence limit on 
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
#' \code{\link{create_volcano_dataset}} 
#' \code{\link{show_volcano_data}} 
#' 
#' @examples 
#' dat <-  read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' vol_dat <- create_volcano_dataset(dat)
#' plot_volcano(vol_dat, show_confidence_limits = T)
#' 
#' @export plot_volcano

plot_volcano <- function(vol_data, 
                         state_1 = "", 
                         state_2 = "",
                         adjust_axes = TRUE,
                         show_confidence_limits = FALSE,
                         confidence_level = 0.98) {
  
  volcano_plot <- ggplot(vol_data, aes(x = D_diff, y = log_p_value)) + 
    geom_point() + 
    geom_errorbar(aes(xmin = D_diff - Uncertainty, xmax = D_diff + Uncertainty), alpha = 0.2) + 
    labs(title = paste0("Volcano Plot ", state_1, " " , state_2),  
         x = "Mass difference [Da]",
         y = "-log(P value)")
  
  if(adjust_axes){
    
    x_max <- ceiling(max(abs(vol_data[["D_diff"]])))
    y_max <- ceiling(max(vol_data[["log_p_value"]])) + 2
    
    volcano_plot <- volcano_plot + 
      coord_cartesian(xlim = c(-x_max, x_max), ylim = c(0, y_max), expand = FALSE) 
    
  }
  
  if(show_confidence_limits){
    
    y_threshold <- -log(1 - confidence_level)
    
    t_value <- qt(c((1 - confidence_level)/2, 1-(1 - confidence_level)/2), df = 2)[2]
    x_threshold <- t_value * mean(vol_data[["Uncertainty"]], na.rm = TRUE)/sqrt(length(vol_data))
    
    volcano_plot <- volcano_plot + 
      geom_segment(aes(x = -x_threshold, xend = -x_threshold, y = y_threshold, yend = Inf), linetype = "dashed", color = "red") +
      geom_segment(aes(x = x_threshold, xend = x_threshold, y = y_threshold, yend = Inf), linetype = "dashed", color = "red") +
      geom_segment(aes(y = y_threshold, yend = y_threshold, x = -Inf, xend = -x_threshold), linetype = "dashed", color = "red") +
      geom_segment(aes(y = y_threshold, yend = y_threshold, x = x_threshold, xend = Inf), linetype = "dashed", color = "red") 
    
  }
  
  return(volcano_plot)
}
