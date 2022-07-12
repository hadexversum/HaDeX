#' State deuterium uptake comparison
#' 
#' @description Comparison plot of deuterium uptake values in time
#' point for biological states.
#' 
#' @importFrom ggplot2 ggplot geom_segment geom_errorbar theme scale_y_continuous
#' 
#' @param uptake_dat data produced by \code{\link{calculate_state_uptake}} 
#' function
#' @param theoretical \code{logical}, indicator if values are 
#' calculated using theoretical controls
#' @param fractional \code{logical}, indicator if values are shown 
#' in fractional form 
#' 
#' @details The function \code{\link{plot_state_comparison}} generates
#' a comparison plot, presenting deuterium uptake values of peptides 
#' from selected protein in selected biological states at one time point 
#' of measurement at once. 
#' On X-axis there is a position in a sequence, with length of a segment 
#' of each peptide representing its length. On Y-axis there 
#' is deuterium uptake in selected form. Error bars represents the combined
#' and propagated uncertainty. 
#' 
#' @return a \code{ggplot} object
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}  
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' comparison_dat <- calculate_state_uptake(dat)
#' plot_state_comparison(comparison_dat)
#' 
#' uptake_dat <- create_uptake_dataset(dat, states = c("CD160", "CD160_HVEM"))
#' plot_state_comparison(uptake_dat, all_times = T)
#' plot_state_comparison(uptake_dat, fractional = T, all_times = T)
#' 
#' @export plot_state_comparison

plot_state_comparison <- function(uptake_dat, 
                                  theoretical = FALSE, 
                                  fractional = FALSE,
                                  line_size = 1.5,
                                  all_times = FALSE,
                                  time_t = NULL){
  
  uptake_dat <- data.table(uptake_dat)
  
  if(is.null(time_t) & !all_times) {time_t <- fcoalesce(attr(uptake_dat, "time_t"), unique(uptake_dat[["Exposure"]])[3] )}
  
  if(!all_times) { uptake_dat <- uptake_dat[Exposure == time_t] }
  
  
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
  
  plot_dat <- data.table(Sequence = uptake_dat[["Sequence"]],
                         Start = uptake_dat[["Start"]],
                         End = uptake_dat[["End"]],
                         Med_Sequence = uptake_dat[["Med_Sequence"]],
                         State = uptake_dat[["State"]],
                         value = uptake_dat[[value]],
                         err_value = uptake_dat[[err_value]],
                         Exposure = uptake_dat[["Exposure"]])
  
  state_comp_plot <- ggplot(data = plot_dat) +
    geom_segment(data = plot_dat, aes(x = Start, y = value, xend = End, yend = value, color = State), size = line_size) +
    geom_errorbar(data = plot_dat, aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = State)) +
    labs(title = title,
         x = "Position in the sequence",
         y = y_label) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  if(all_times){
    
    state_comp_plot <- state_comp_plot + 
      facet_wrap(~Exposure)
    
  }
  
  return(HaDeXify(state_comp_plot))
  
}


