#' Differential chiclet plot
#'
#' @param diff_uptake_dat data produced by
#' \code{\link{create_diff_uptake_dataset}} function.
#' @param diff_p_uptake_dat ...
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param show_uncertainty \code{logical}, determines if the
#' uncertainty is shown.
#' @param show_houde_interval \code{logical}, determines if houde interval is shown.
#' @param show_tstud_confidence \code{logical}, determines if t-Student test validity 
#' is shown.
#' @param confidence_level confidence level for the test, from range [0, 1].
#' Important if selected show_confidence_limit.
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
#'
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' plot_differential_chiclet(diff_uptake_dat)
#' plot_differential_chiclet(diff_uptake_dat, show_houde_interval = T)
#' 
#' diff_p_uptake_dat <- create_p_diff_uptake_dataset(dat)
#' plot_differential_chiclet(diff_p_uptake_dat = diff_p_uptake_dat, show_tstud_confidence = T)
#' plot_differential_chiclet(diff_p_uptake_dat = diff_p_uptake_dat, show_tstud_confidence = T, show_houde_interval = T) 
#' 
#' @export plot_differential_chiclet

plot_differential_chiclet <- function(diff_uptake_dat = NULL, 
                                      diff_p_uptake_dat = NULL, 
                                      theoretical = FALSE,
                                      fractional = FALSE,
                                      show_houde_interval = FALSE,
                                      show_tstud_confidence = FALSE,
                                      confidence_level = 0.98,
                                      show_uncertainty = FALSE){
  
  if (show_tstud_confidence) {
    
    if(is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } else { diff_uptake_dat <- diff_p_uptake_dat }
    
  } else {
    
    if(is.null(diff_uptake_dat) & is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } 
    
  }
  
  ##
  
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
  
  diff_uptake_dat <- as.data.table(diff_uptake_dat)
  
  plot_dat <- data.frame(ID = diff_uptake_dat[["ID"]],
                         Exposure = as.factor(diff_uptake_dat[["Exposure"]]),
                         value = diff_uptake_dat[[value]],
                         err_value = diff_uptake_dat[[err_value]],
                         Sequence = diff_uptake_dat[["Sequence"]],
                         Start = diff_uptake_dat[["Start"]],
                         End = diff_uptake_dat[["End"]])
  
  attr(plot_dat, "n_rep") <- attr(diff_uptake_dat, "n_rep")
  
  min_du <- min(plot_dat[["value"]])
  max_du <- max(plot_dat[["value"]])
  
  chiclet_differential_plot <- ggplot(plot_dat, aes(y = Exposure, x = ID)) +
    geom_tile(aes(fill = value)) +
    geom_tile(data = subset(plot_dat, is.na(value)), fill = "gray95") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", guide = guide_legend(keywidth = 3), limits = c(min_du, max_du)) + #       limits = c(min(facetgrid$value), max(facetgrid$value)))
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
  
  if(show_houde_interval){
    
    # t_value <- qt(c((1 - confidence_level)/2, 1-(1 - confidence_level)/2), df = 2)[2]
    # x_threshold <- t_value * mean(plot_dat[["err_value"]], na.rm = TRUE)/sqrt(length(plot_dat))
    
    x_threshold <- calculate_confidence_limit_values(plot_dat, 
                                                     confidence_level = confidence_level,
                                                     n_rep = attr(diff_uptake_dat, "n_rep"))[2]
    
    chiclet_differential_plot <- chiclet_differential_plot +
      geom_tile(data = subset(plot_dat, abs(value) < x_threshold), fill = "grey91")
    
  }
  
  if(show_tstud_confidence){
    
    alpha <- -log(1 - confidence_level)
    
    #### datatable extra
    diff_uptake_dat <- data.table(diff_uptake_dat)
    
    diff_uptake_dat[, `:=`(valid = log_p_value >= alpha, 
                           Exposure = as.factor(Exposure))]
      
    diff_uptake_dat <- merge(diff_uptake_dat, plot_dat, by = c("Sequence", "Start", "End", "Exposure", "ID"))
    
    chiclet_differential_plot <- chiclet_differential_plot +
      geom_tile(data = subset(diff_uptake_dat, !valid), aes(x = ID, y = Exposure), fill = "grey89") +
      geom_tile(data = subset(diff_uptake_dat, is.na(valid)), aes(x = ID, y = Exposure), fill = "grey56")
    
  }
  
  return(HaDeXify(chiclet_differential_plot))
  
}
