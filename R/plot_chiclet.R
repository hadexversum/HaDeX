#' Chiclet deuterium uptake plot
#'
#' @description Chiclet plot of deuterium uptake values in time
#' for one biological state.
#' 
#' @importFrom ggplot2 geom_tile scale_fill_gradient2 guide_legend element_rect
#' 
#' @param chiclet_dat produced by \code{\link{create_state_uptake_dataset}}
#' function. 
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param show_uncertainty \code{logical}, determines if the
#' uncertainty is shown. 
#' @inheritParams plot_butterfly
#' 
#' @details Function \code{\link{plot_chiclet}} produces a chiclet
#' plot based on the same dataset as butterfly plot, as it is the different
#' form of presenting the same data. On X-axis there is a peptide ID. On 
#' Y-axis are time points of measurement. Each tile for a peptide in time has
#' a color value representing the deuterium uptake, in a form based on 
#' provided criteria (e.q. fractional). Each tile has a plus sign, which size 
#' represent the uncertainty of measurement for chosen value.
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{create_state_uptake_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' state_uptake_dat <- create_state_uptake_dataset(dat)
#' plot_chiclet(state_uptake_dat)
#' 
#' @export plot_chiclet

plot_chiclet <- function(uptake_dat, 
                         theoretical = FALSE, 
                         fractional = FALSE,
                         show_uncertainty = FALSE,
                         interactive = getOption("hadex_use_interactive_plots")){
  
  state <- unique(uptake_dat[["State"]])
  
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
  
  plot_dat <- data.table(ID = uptake_dat[["ID"]],
                         Exposure = as.factor(uptake_dat[["Exposure"]]),
                         value = uptake_dat[[value]],
                         err_value = uptake_dat[[err_value]],
                         Sequence = uptake_dat[["Sequence"]],
                         Start = uptake_dat[["Start"]],
                         End = uptake_dat[["End"]])
  
  chosen_tile_geom <- if (interactive) ggiraph::geom_tile_interactive(
    aes(tooltip = glue(
      "{Sequence}
       Position: {Start}-{End}
       ID: {ID}
       Value: {round(value, 2)}
       Exposure: {Exposure} min"
    ))
  ) else geom_tile()
  
  chiclet_plot <- ggplot(plot_dat, aes(y = Exposure, x = ID, fill = value)) +
    chosen_tile_geom +
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
