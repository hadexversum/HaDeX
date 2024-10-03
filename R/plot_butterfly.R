#' Butterfly deuterium uptake plot
#' 
#' @description Butterfly plot of deuterium uptake values in time
#' for one biological state.
#' 
#' @param uptake_dat data produced by \code{\link{create_state_uptake_dataset}} 
#' function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param uncertainty_type type of presenting uncertainty, possible values:
#' "ribbon", "bars" or "bars + line".
#' @param interactive \code{logical}, whether plot should have an interactive 
#' layer created with with ggiraph, which would add tooltips to the plot in an
#' interactive display (HTML/Markdown documents or shiny app).
#' 
#' @details Function \code{\link{plot_butterfly}} generates butterfly plot
#' based on provided data and parameters. On X-axis there is peptide ID. On the Y-axis
#' there is deuterium uptake in chosen form. Data from multiple time points of 
#' measurement is presented.
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{create_state_uptake_dataset}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' state_uptake_dat <- create_state_uptake_dataset(dat)
#' plot_butterfly(state_uptake_dat)
#' 
#' @export plot_butterfly
#' @importFrom ggiraph geom_point_interactive
#' @importFrom glue glue

plot_butterfly <- function(uptake_dat, 
                           theoretical = FALSE, 
                           fractional = FALSE,
                           uncertainty_type = "ribbon",
                           interactive = getOption("hadex_use_interactive_plots")){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars", "bars + line"))
  state <- unique(uptake_dat[["State"]])
  
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
  
  plot_dat <- data.table(ID = uptake_dat[["ID"]],
                         Exposure = as.factor(uptake_dat[["Exposure"]]),
                         value = uptake_dat[[value]],
                         err_value = uptake_dat[[err_value]],
                         Sequence = uptake_dat[["Sequence"]],
                         Start = uptake_dat[["Start"]],
                         End = uptake_dat[["End"]])
  
  chosen_geom_point <- if (interactive) geom_point_interactive( 
    aes(tooltip = glue(
      "{Sequence}
       Position: {Start}-{End}
       Value: {round(value, 2)}
       Exposure: {Exposure} min"
    ))
  ) else geom_point()
  
  chosen_uncertainty_geom <- if(uncertainty_type == "ribbon") {
    geom_ribbon(alpha = 0.5, size = 0, linetype = "blank")
  } else { geom_errorbar(width = 0.25, alpha = 0.5) }
  
  butterfly_plot <- ggplot(
    plot_dat, 
    aes(
      x = ID, 
      y = value, 
      group = Exposure, 
      color = Exposure, 
      fill = Exposure,
      ymin = value - err_value, 
      ymax = value + err_value
    )) +
    chosen_uncertainty_geom +
    chosen_geom_point +
    coord_cartesian(ylim = c(0, NA)) +
    labs(x = "Peptide ID",
         y = y_label) +
    theme(legend.position = "bottom")
  
  if(uncertainty_type == "bars + line") butterfly_plot <- butterfly_plot + geom_line()
  
  return(HaDeXify(butterfly_plot))
  
}
