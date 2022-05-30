#' Plot differential uptake curve
#' 
#' @description 
#' 
#' @param diff_uptake_dat produced by \code{\link{create_diff_uptake_dataset}} function
#' @param sequence 
#' @param theoretical \code{logical}, determines if plot shows theoretical values.
#' @param fractional \code{logical}, determines if plot shows fractional values.
#' @param uncertainty_type type of presenting uncertainty, possible values:
#' "ribbon", "bars" or "bars + line".
#' @param log_x \code{logical}, determines if x axis shows logarithmic values.
#' @param show_houde_interval \code{logical}, determines if houde interval is shown.
#' @param show_tstud_confidence \code{logical}, determines if t-Student test validity 
#' is shown.
#' 
#' @details Currently there is no possibility to plot multiple peptides on the plot.
#' 
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{create_diff_uptake_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' plot_differential_uptake_curve(diff_uptake_dat = diff_uptake_dat, sequence = "LCKDRSGDCSPETSLKQL")
#' 
#' diff_p_uptake_dat <- create_p_diff_uptake_dataset(dat)
#' plot_differential_uptake_curve(diff_p_uptake_dat = diff_p_uptake_dat, sequence = "LCKDRSGDCSPETSLKQL", show_houde_interval = TRUE)
#' plot_differential_uptake_curve(diff_p_uptake_dat = diff_p_uptake_dat, sequence = "LCKDRSGDCSPETSLKQL", show_houde_interval = TRUE, show_tstud_confidence = TRUE)
#' plot_differential_uptake_curve(diff_p_uptake_dat = diff_p_uptake_dat, sequence = "LCKDRSGDCSPETSLKQL", show_tstud_confidence = TRUE)
#' 
#' @export plot_differential_uptake_curve

plot_differential_uptake_curve <- function(diff_uptake_dat = NULL,
                                           diff_p_uptake_dat = NULL,
                                           sequence = NULL,
                                           theoretical = FALSE,
                                           fractional = FALSE,
                                           uncertainty_type = "ribbon",
                                           log_x = TRUE,
                                           show_houde_interval = FALSE,
                                           show_tstud_confidence = FALSE){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars", "bars + line"))
  
  if(is.null(sequence)){ sequence <- diff_uptake_dat[["Sequence"]][1] }
  
  states <- paste0(attr(diff_uptake_dat, "state_1"), "-", attr(diff_uptake_dat, "state_2"))
  
  ##
  
  if (show_tstud_confidence) {
    
    if(is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } else { diff_uptake_dat <-  filter(diff_p_uptake_dat, Sequence == sequence) }
    
  } else {
    
    if(is.null(diff_uptake_dat) & is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } else  { 
      
      if(is.null(diff_uptake_dat)) {diff_uptake_dat <- filter(diff_p_uptake_dat, Sequence == sequence) } else {
        
        diff_uptake_dat <- filter(diff_uptake_dat, Sequence == sequence)
        
      }
      
    }
    
  }
  
  diff_uptake_dat <- filter(diff_uptake_dat, Exposure < 99999) 
  
  ##
  
  if (theoretical){
    
    title <- "Theoretical differential uptake curve"
    
    if (fractional){
      
      value <- "diff_theo_frac_deut_uptake"
      err_value <- "err_diff_theo_frac_deut_uptake"
      y_label <- "Fractional differential uptake [%]"
      
    } else {
      
      value <- "diff_theo_deut_uptake"
      err_value <- "err_diff_theo_deut_uptake"
      y_label <- "Differential uptake [Da]"
      
    }
    
  } else {
    
    title <- "Differential uptake curve"
    
    if (fractional){
      
      value <- "diff_frac_deut_uptake"
      err_value <- "err_diff_frac_deut_uptake"
      y_label <- "Fractional differential uptake [%]"
      
    } else {
      
      value <- "diff_deut_uptake"
      err_value <- "err_diff_deut_uptake"
      y_label <- "Differential uptake [Da]"
      
    }
    
  }
  
  plot_dat <- data.frame(Sequence = diff_uptake_dat[["Sequence"]],
                         Start = diff_uptake_dat[["Start"]],
                         End = diff_uptake_dat[["End"]],
                         Exposure = diff_uptake_dat[["Exposure"]],
                         value = diff_uptake_dat[[value]],
                         err_value = diff_uptake_dat[[err_value]])
  
  diff_kin_plot <- plot_dat %>% 
    ggplot(aes(x = Exposure, y = value, group = Sequence)) +
    geom_point(aes(shape = Sequence, color = states), size = 2) + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Time points [min]", 
         y = y_label,
         title = title)
  
  if(log_x){ err_width = 0.1 } else { err_width = 5 }
  
  if(uncertainty_type == "ribbon"){
    
    diff_kin_plot <- diff_kin_plot +
      geom_ribbon(aes(ymin = value - err_value, ymax = value + err_value, fill = Sequence), alpha = 0.15) +
      geom_line(aes(color = states)) 
    
  } else if (uncertainty_type == "bars") {
    
    diff_kin_plot <- diff_kin_plot +
      geom_errorbar(aes(x = Exposure, ymin = value - err_value, ymax = value + err_value, color = states),
                    width = err_width)
    
  } else if (uncertainty_type == "bars + line"){
    
    diff_kin_plot <- diff_kin_plot +
      geom_errorbar(aes(x = Exposure, ymin = value - err_value, ymax = value + err_value, color = states),
                    width = err_width) + 
      geom_line(aes(color = Sequence))
    
  }
  
  if(show_houde_interval){
    
    houde_intervals <- diff_uptake_dat %>%
      calculate_confidence_limit_values(confidence_level = attr(diff_uptake_dat, "confidence_level"),
                                        theoretical = theoretical,
                                        fractional = fractional)
    
    diff_kin_plot <- diff_kin_plot +
      geom_hline(yintercept = houde_intervals[2], linetype = "dashed", color = "red")
    
  }
  
  if(show_tstud_confidence){
    
    alpha <- -log(1 - attr(diff_uptake_dat, "confidence_level"))
    
    diff_uptake_dat <- mutate(diff_uptake_dat, valid = log_p_value >= alpha) %>%
      merge(plot_dat, by = c("Sequence", "Start", "End", "Exposure"))
    
    diff_kin_plot <- diff_kin_plot +
      geom_point(data = subset(diff_uptake_dat, !valid), aes(x = Exposure, y = value), shape = 13, size = 2)
    
  }
  
  if(log_x){
    
    diff_kin_plot <- diff_kin_plot + 
      scale_x_log10()
    
  }
  
  return(HaDeXify(diff_kin_plot))
  
}