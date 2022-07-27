#' Differential plot
#'
#' @description Woods plot of differential deuterium uptake values 
#' between two biological states in time point.
#' 
#' @importFrom ggplot2 facet_wrap
#' @importFrom data.table as.data.table
#' 
#' @param diff_uptake_dat produced by \code{\link{create_diff_uptake_dataset}} 
#' function.
#' @param diff_p_uptake_dat produced by \code{\link{create_p_diff_uptake_dataset}} 
#' function.
#' @param skip_amino \code{integer}, indicator how many aminos from the N-terminus 
#' should be omitted
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param show_houde_interval \code{logical}, determines if houde interval is shown.
#' @param hide_houde_insignificant \code{logical}, determines if statistically
#' insignificant values using Houde test are hidden on the plot.
#' @param show_tstud_confidence \code{logical}, determines if t-Student test validity 
#' is shown.
#' @param hide_tstud_insignificant \code{logical}, determines if statistically
#' insignificant values using t-Student test are hidden on the plot.
#' @param confidence_level confidence level for the test, from range [0, 1].
#' @param time_t time point of measurement, if only one should be displayed
#' on the plot.
#' @param all_times \code{logical}, determines if all the time points from the
#' supplied data should be displayed on the plots next to each other.
#' @param line_size line size of the lines displayed on the plot.
#'
#' @details Function \code{\link{plot_differential}} presents
#' provided data in a form of differential (Woods) plot. The plot shows
#' difference in exchange for two biological states, selected in
#' generation of dataset at one time point of measurement. On X-axis
#' there is a position in a sequence, with length of a segment of each
#' peptide representing its length. On Y-axis there
#' is deuterium uptake difference in chosen form. Error bars represents
#' the combined and propagated uncertainty.
#' For Woods Plot there is available Houde test and t-Student test to 
#' see the statistically significant peptides. Selecting both of them 
#' simultaneously results in hybrid testing, as described in Weis et al.
#' The statistically significant values are in color (red if the 
#' difference is positive, blue if negative), and the insignificant values are 
#' grey. 
#'
#' @return a \code{\link{ggplot}} object.
#'
#' @references Hageman, T. S. & Weis, D. D. Reliable Identification of Significant 
#' Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements 
#' Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
#' 
#' @seealso
#' \code{\link{create_diff_uptake_dataset}}
#' \code{\link{show_diff_uptake_data}}
#'
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' plot_differential(diff_uptake_dat = diff_uptake_dat, time_t = 0.167) 
#' plot_differential(diff_uptake_dat = diff_uptake_dat, time_t = 0.167, skip_amino = 0) 
#' plot_differential(diff_uptake_dat = diff_uptake_dat, time_t = 0.167, line_size = 1) 
#' plot_differential(diff_uptake_dat = diff_uptake_dat, all_times = T)
#' plot_differential(diff_uptake_dat = diff_uptake_dat, all_times = T, show_houde_interval = T)
#' plot_differential(diff_uptake_dat = diff_uptake_dat, all_times = T, show_houde_interval = T, hide_houde_insignificant = T)
#' 
#' diff_p_uptake_dat <- create_p_diff_uptake_dataset(dat)
#' plot_differential(diff_p_uptake_dat = diff_p_uptake_dat, all_times = T, show_tstud_confidence = T)
#' plot_differential(diff_p_uptake_dat = diff_p_uptake_dat, all_times = T, show_tstud_confidence = T, show_houde_interval = T)
#' plot_differential(diff_p_uptake_dat = diff_p_uptake_dat, show_tstud_confidence = T, show_houde_interval = T, all_times = F)
#' plot_differential(diff_p_uptake_dat = diff_p_uptake_dat, show_tstud_confidence = T, show_houde_interval = T, all_times = F, hide_houde_insignificant = T)
#' plot_differential(diff_p_uptake_dat = diff_p_uptake_dat, show_tstud_confidence = T, show_houde_interval = T, all_times = F, hide_houde_insignificant = T, hide_tstud_insignificant = T)
#' 
#' @export plot_differential

plot_differential <- function(diff_uptake_dat = NULL,
                              diff_p_uptake_dat =  NULL, 
                              skip_amino = 0,  
                              time_t = NULL,
                              theoretical = FALSE,
                              fractional = FALSE, 
                              show_houde_interval = FALSE,
                              hide_houde_insignificant = FALSE,
                              show_tstud_confidence = FALSE,
                              hide_tstud_insignificant = FALSE, 
                              confidence_level = 0.98,
                              all_times = FALSE,
                              line_size = 1.5){
  
  ## conditions
  
  if (show_tstud_confidence | hide_tstud_insignificant) {
    
    if(is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } else { diff_uptake_dat <- diff_p_uptake_dat  }
    
  } else {
    
    if(is.null(diff_uptake_dat)){
      
      if(is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } else  { 
        
        diff_uptake_dat <- diff_p_uptake_dat 
        
      }
    }
  }
  
  if(is.null(time_t) & !all_times) {
    
    if(is.null(attr(diff_uptake_dat, "time_t"))){ time_t <- unique(diff_uptake_dat[["Exposure"]])[3] }
    else { time_t <- attr(diff_uptake_dat, "time_t")}
    
  }  
  
  ##
  diff_uptake_dat <- as.data.table(diff_uptake_dat)
  ##
  
  if(!all_times) { 
    diff_uptake_dat <- diff_uptake_dat[Exposure == time_t]
  }
  
  ##
  
  if(skip_amino > 0) { diff_uptake_dat[, Start := Start + skip_amino] } 
  
  h_interval <- calculate_confidence_limit_values(diff_uptake_dat = diff_uptake_dat,
                                                  confidence_level = confidence_level,
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
  
  if(!all_times) {title <- paste0(title, " in ", time_t, " min") }
  
  plot_dat <- data.table(Protein = diff_uptake_dat[["Protein"]],
                         Sequence = diff_uptake_dat[["Sequence"]],
                         Start = diff_uptake_dat[["Start"]],
                         End = diff_uptake_dat[["End"]],
                         Med_Sequence = diff_uptake_dat[["Med_Sequence"]],
                         Modification = diff_uptake_dat[["Modification"]],
                         value = diff_uptake_dat[[value]],
                         err_value = diff_uptake_dat[[err_value]],
                         Exposure = diff_uptake_dat[["Exposure"]])
  
  if(hide_houde_insignificant){
    
    plot_dat <- plot_dat[abs(value) >= h_interval[2]]
    
  }
  
  if(hide_tstud_insignificant){
    
    alpha <- -log(1 - attr(diff_uptake_dat, "confidence_level"))
    
    diff_uptake_dat[["valid"]] <- diff_uptake_dat[["log_p_value"]] >= alpha
    
    ### extra data.table
    diff_uptake_dat <- data.table(merge(diff_uptake_dat, plot_dat, by = c("Sequence", "Start", "End", "Med_Sequence", "Protein", "Exposure")))
    
    plot_dat <- diff_uptake_dat[(valid)]
    
  }
  
  if(show_houde_interval){
    
    plot_dat[, colour := fcase(value > h_interval[2], "firebrick1",
                               value < h_interval[1], "deepskyblue1",
                               default = "azure3")]
    
    
    differential_plot <- ggplot(plot_dat) +
      geom_segment(aes(x = Start, y = value, xend = End, yend = value, color = colour), size = line_size) +
      geom_errorbar(aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = colour)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
      ## intervals
      geom_hline(aes(yintercept = h_interval[1], linetype = paste0(" Confidence interval ", confidence_level*100, "% : ", round(h_interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
      geom_hline(aes(yintercept = h_interval[2], linetype = paste0(" Confidence interval ", confidence_level*100, "% : ", round(h_interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
      scale_linetype_manual(values = c("dashed", "dotdash")) +
      ## other
      scale_colour_identity() +
      labs(title = title,
           x = "Position in the sequence",
           y = y_label,
           linetype = "") +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "vertical")
    
  } else {
    
    plot_dat[, colour := fcase(value > 0, "firebrick1",
                               value < 0, "deepskyblue1",
                               default = "azure3")]
    
    differential_plot <- ggplot(plot_dat) +
      geom_segment(aes(x = Start, y = value, xend = End, yend = value, color = colour), size = line_size) +
      geom_errorbar(aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = colour)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
      ## other
      scale_colour_identity() +
      labs(title = title,
           x = "Position in the sequence",
           y = y_label) +
      theme(legend.position = "none")
    
  }
  
  if(show_tstud_confidence){
    
    if(!hide_tstud_insignificant){
      
      alpha <- -log(1 - attr(diff_uptake_dat, "confidence_level"))
      
      diff_uptake_dat[["valid"]] <- diff_uptake_dat[["log_p_value"]] >= alpha
      
      ### extra data.table
      diff_uptake_dat <- data.table(merge(diff_uptake_dat, plot_dat, by = c("Sequence", "Start", "End", "Med_Sequence", "Protein", "Exposure", "Modification")))
      
    }
    
    differential_plot <- differential_plot +
      geom_segment(data = subset(diff_uptake_dat, !valid), aes(x = Start, y = value, xend = End, yend = value), color = "grey77", size = line_size) +
      geom_errorbar(data = subset(diff_uptake_dat, !valid), aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value), color = "grey77") 
    
  }
  
  if(all_times) {
    
    differential_plot <- differential_plot + 
      facet_wrap(~Exposure)
    
  }
  
  return(HaDeXify(differential_plot))
  
}



