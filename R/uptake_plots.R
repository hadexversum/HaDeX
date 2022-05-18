#' Plot state comparison
#' 
#' @importFrom ggplot2 ggplot geom_segment geom_errorbar theme scale_y_continuous
#' 
#' @param dat data produced by \code{\link{calculate_state_uptake}} function.
#' @param theoretical \code{logical}, determines if values are theoretical. 
#' @param fractional \code{logical}, determines if values are fractional.
#' 
#' @details Function \code{\link{plot_state_comparison}} presents provided 
#' data in a form of comparison plot, for peptides for chosen protein in chosen states,
#' at one time point of measurement at once. On X-axis there is a position in a sequence, 
#' with length of a segment of each peptide representing its length. On Y-axis there 
#' is deuterium uptake in chosen form. Error bars represents the combined and propagated
#' uncertainty. 
#' This plot is visible in GUI. 
#' 
#' @return a \code{ggplot} object.
#' 
#' @seealso 
#' \code{\link{calculate_state_uptake}}  
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' comparison_dat <- calculate_state_uptake(dat)
#' plot_state_comparison(comparison_dat)
#' 
#' @export plot_state_comparison

plot_state_comparison <- function(dat, 
                                  theoretical = FALSE, 
                                  fractional = FALSE){
  
  if (theoretical) {
    
    title <- "Theoretical comparison plot"
    
    if (fractional) {
      
      # theoretical & fractional
      value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      y_label <- "fractional deuterium uptake [%]"
      
    } else {
      
      # theoretical & absolute
      value <- "theo_deut_uptake"
      err_value <- "err_theo_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      
    } 
    
  } else {
    
    title <- "Comparison plot"
    
    if (fractional) {
      
      # experimantal & fractional
      value <- "frac_deut_uptake"
      err_value <- "err_frac_deut_uptake"
      y_label <- "fractional deuterium uptake [%]"
      
    } else {
      
      # experimental & absolute 
      value <- "deut_uptake"
      err_value <- "err_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      
    }
    
  }
  
  plot_dat <- data.frame(Sequence = dat[["Sequence"]],
                         Start = dat[["Start"]],
                         End = dat[["End"]],
                         Med_Sequence = dat[["Med_Sequence"]],
                         State = dat[["State"]],
                         value = dat[[value]],
                         err_value = dat[[err_value]])
  
  state_comp_plot <- ggplot(data = plot_dat) +
    geom_segment(data = plot_dat, aes(x = Start, y = value, xend = End, yend = value, color = State)) +
    geom_errorbar(data = plot_dat, aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = State)) +
    labs(title = title,
         x = "Position in the sequence",
         y = y_label) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  return(HaDeXify(state_comp_plot))
  
}

#' Butterfly plot
#' 
#' @param uptake_dat data produced by \code{\link{create_state_uptake_dataset}} 
#' function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param uncertainty_type type of presenting uncertainty, possible values:
#' "ribbon", "bars" or "bars + line".
#' 
#' @details Function \code{\link{plot_butterfly}} generates butterfly plot
#' based on provided data and parameters. On X-axis there is peptide ID. On the Y-axis
#' there is deuterium uptake in chosen form. Data from multiple time points of 
#' measurement is presented.
#' This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{create_state_uptake_dataset}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' butterfly_dat <- create_state_uptake_dataset(dat)
#' plot_butterfly(butterfly_dat)
#' 
#' @export plot_butterfly

plot_butterfly <- function(butterfly_dat, 
                           theoretical = FALSE, 
                           fractional = FALSE,
                           uncertainty_type = "ribbon"){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars", "bars + line"))
  state <- unique(butterfly_dat[["State"]])
  
  if (theoretical) {
    
    if (fractional) {
      
      # theoretical & fractional
      value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake [%]"
      title <- paste0("Theoretical butterfly plot for ", state, " state")
      
    } else {
      
      # theoretical & absolute
      value <- "theo_deut_uptake"
      err_value <- "err_theo_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      title <- paste0("Theoretical butterfly plot for ", state, " state")
      
    }
    
  } else {
    
    if (fractional) {
      
      # experimental & fractional
      value <- "frac_deut_uptake"
      err_value <- "err_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake [%]"
      title <- paste0("Butterfly plot for ", state, " state")
      
    } else {
      
      # experimental & absolute
      value <- "deut_uptake"
      err_value <- "err_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      title <- paste0("Butterfly plot for ", state, " state")
      
    }
    
  }
  
  plot_dat <- data.frame(ID = butterfly_dat[["ID"]],
                         Exposure = as.factor(butterfly_dat[["Exposure"]]),
                         value = butterfly_dat[[value]],
                         err_value = butterfly_dat[[err_value]],
                         Sequence = butterfly_dat[["Sequence"]],
                         Start = butterfly_dat[["Start"]],
                         End = butterfly_dat[["End"]])
  
  butterfly_plot <- ggplot(plot_dat, aes(x = ID, y = value, color = Exposure)) +
    geom_point(aes(group = Exposure, color = Exposure)) +
    coord_cartesian(ylim = c(0, NA)) +
    labs(x = "Peptide ID",
         y = y_label) +
    theme(legend.position = "bottom")
  
  if(uncertainty_type == "ribbon"){
    
    butterfly_plot <- butterfly_plot +
      geom_ribbon(aes(x = ID, ymin = value - err_value, ymax = value + err_value, fill = Exposure), alpha = 0.5, size = 0, linetype = "blank")
    
  } else if (uncertainty_type == "bars") {
    
    butterfly_plot <- butterfly_plot +
      geom_errorbar(aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5)
    
  } else if (uncertainty_type == "bars + line"){
    
    butterfly_plot <- butterfly_plot +
      geom_errorbar(aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5) +
      geom_line()
    
  }
  
  return(HaDeXify(butterfly_plot))
  
}

#' Generate chiclet plot
#'  
#' @importFrom ggplot2 geom_tile scale_fill_gradient2 guide_legend element_rect
#' 
#' @param chiclet_dat produced by \code{\link{create_state_uptake_dataset}}
#' function. 
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param show_uncertainty \code{logical}, determines if the
#' uncertainty is shown. 
#' 
#' @details Function \code{\link{plot_chiclet}} produces a chiclet
#' plot based on the same dataset as butterfly plot, as it is the different
#' form of presenting the same data. On X-axis there is a peptide ID. On 
#' Y-axis are time points of measurement. Each tile for a peptide in time has
#' a color value representing the deuterium uptake, in a form based on 
#' provided criteria (e.q. fractional). Each tile has a plus sign, which size 
#' represent the uncertainty of measurement for chosen value.
#' This plot is visible in GUI.
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{create_state_uptake_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' chic_dat <- create_state_uptake_dataset(dat)
#' plot_chiclet(chic_dat)
#' 
#' @export plot_chiclet

plot_chiclet <- function(chiclet_dat, 
                         theoretical = FALSE, 
                         fractional = FALSE,
                         show_uncertainty = FALSE){
  
  state <- unique(chiclet_dat[["State"]])
  
  if (theoretical) {
    
    if (fractional) {
      
      # theoretical & fractional
      value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      title <- paste0("Theoretical chiclet plot for ", state, " state")
      fill <- "Fractional DU"
      
    } else {
      
      # theoretical & absolute
      value <- "theo_deut_uptake"
      err_value <- "err_theo_deut_uptake"
      title <- paste0("Theoretical chiclet plot for ", state, " state")
      fill <- "DU"
    }
    
  } else {
    
    if (fractional) {
      
      # experimental & fractional
      value <- "frac_deut_uptake"
      err_value <- "err_frac_deut_uptake"
      title <- paste0("Chiclet plot for ", state, " state")
      fill <- "Fractional DU"
      
    } else {
      
      # experimental & absolute
      value <- "deut_uptake"
      err_value <- "err_deut_uptake"
      title <- paste0("Chiclet plot for ", state, " state")
      fill <- "DU"
      
    }
    
  }
  
  plot_dat <- data.frame(ID = chiclet_dat[["ID"]],
                         Exposure = as.factor(chiclet_dat[["Exposure"]]),
                         value = chiclet_dat[[value]],
                         err_value = chiclet_dat[[err_value]],
                         Sequence = chiclet_dat[["Sequence"]],
                         Start = chiclet_dat[["Start"]],
                         End = chiclet_dat[["End"]])
  
  chiclet_plot <- ggplot(plot_dat, aes(y = Exposure, x = ID)) +
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
    
    chiclet_plot <- chiclet_plot +
      geom_point(aes(size = err_value), shape = 3) + 
      labs(size = "Err")
    
  } 
  
  return(HaDeXify(chiclet_plot))
  
}
