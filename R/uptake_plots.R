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
#' \code{\link{generate_butterfly_data}}
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
  
  return(butterfly_plot)
  
}


