#' Differential plot
#'
#' @param diff_uptake_dat produced by \code{\link{create_diff_uptake_dataset}} function.
#' @param diff_p_uptake_dat ...
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param show_houde_interval \code{logical}, determines if houde interval is shown.
#' @param show_tstud_confidence \code{logical}, determines if t-Student test validity 
#' is shown.
#' @param confidence_level confidence level for the test, from range [0, 1].
#'
#' @details Function \code{\link{plot_differential}} presents
#' provided data in a form of differential (Woods) plot. The plot shows
#' difference in exchange for two biological states, selected in
#' generation of dataset at one time point of measurement .On X-axis
#' there is a position in a sequence, with length of a segment of each
#' peptide representing its length. On Y-axis there
#' is deuterium uptake difference in chosen form. Error bars represents
#' the combined and propagated uncertainty.
#' The confidence limits based on provided confidence levels are shown
#' on the plot. The statistically significant values are in color (red if the 
#' difference is positive, blue if negative), and the insignificant values are 
#' grey. There are two confidence limits for comparison of the results, but
#' there is possibility to plot only one confidence limit, if the confidence
#' levels are the same.
#' This plot is visible in GUI.
#'
#' @return a \code{\link{ggplot}} object.
#'
#' @seealso
#' \code{\link{create_diff_uptake_dataset}}
#' \code{\link{show_diff_uptake_data}}
#'
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' plot_differential(diff_uptake_dat)
#'
#' @export plot_differential

plot_differential <- function(diff_uptake_dat = NULL,
                              diff_p_uptake_dat =  NULL, 
                              time_t = NULL,
                              theoretical = FALSE,
                              fractional = FALSE,
                              show_houde_interval = FALSE,
                              show_tstud_confidence = FALSE,
                              confidence_level = 0.98){
  
  ## conditions
  
  if (show_tstud_confidence) {
    
    if(is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } else { diff_uptake_dat <- diff_p_uptake_dat %>% filter(Exposure == attr(diff_uptake_dat, "time_t")) }
    
  } else {
    
    if(is.null(diff_uptake_dat) & is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } else  { 
      
       diff_uptake_dat <- diff_p_uptake_dat %>% filter(Exposure == attr(diff_uptake_dat, "time_t"))
      
    }
  }


  ##
  
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

  plot_dat <- data.frame(Protein = diff_uptake_dat[["Protein"]],
                         Sequence = diff_uptake_dat[["Sequence"]],
                         Start = diff_uptake_dat[["Start"]],
                         End = diff_uptake_dat[["End"]],
                         Med_Sequence = diff_uptake_dat[["Med_Sequence"]],
                         value = diff_uptake_dat[[value]],
                         err_value = diff_uptake_dat[[err_value]])
  
  if(show_houde_interval){
    
    differential_plot <- mutate(plot_dat, colour = case_when(
                                                        value < h_interval[1] ~ "deepskyblue1",
                                                        value > h_interval[2] ~ "firebrick1",
                                                        TRUE ~ "azure3")) %>%
      ggplot() +
      geom_segment(aes(x = Start, y = value, xend = End, yend = value, color = colour)) +
      geom_errorbar(aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = colour)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
      ## intervals
      geom_hline(aes(yintercept = h_interval[1], linetype = paste0(" Confidence interval ", confidence_level*100, "% : ", round(h_interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
      geom_hline(aes(yintercept = h_interval[2], linetype = paste0(" Confidence interval ", confidence_level*100, "% : ", round(h_interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
      scale_linetype_manual(values = c("dashed", "dotdash")) +
      ## other
      scale_colour_identity() +
      labs(title = title,
           x_label = "Position in the sequence",
           y_label = y_label) +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "vertical")
    
  } else {
    
    differential_plot <- plot_dat %>%
      mutate(colour = case_when(
                        value > 0 ~ "deepskyblue3",
                        value < 0 ~ "firebrick3",
                        TRUE ~ "azure3")) %>%
      ggplot() +
      geom_segment(aes(x = Start, y = value, xend = End, yend = value, color = colour )) +
      geom_errorbar(aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = colour)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
      ## other
      labs(title = title,
           x_label = "Position in the sequence",
           y_label = y_label) +
      theme(legend.position = "none")
    
  }
  
  if(show_tstud_confidence){
    
    alpha <- -log(1 - attr(diff_uptake_dat, "confidence_level"))
    
    diff_uptake_dat <- mutate(diff_uptake_dat, valid = log_p_value >= alpha) %>%
      merge(plot_dat, by = c("Sequence", "Start", "End", "Med_Sequence", "Protein"))
    
    differential_plot <- differential_plot +
      geom_segment(data = subset(diff_uptake_dat, !valid), aes(x = Start, y = value, xend = End, yend = value), color = "grey77") +
      geom_errorbar(data = subset(diff_uptake_dat, !valid), aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value), color = "grey77") 
    
    if(!show_houde_interval) { differential_plot <- differential_plot + theme(legend.position = "none") }
    
  }
  
  return(HaDeXify(differential_plot))
  
}



