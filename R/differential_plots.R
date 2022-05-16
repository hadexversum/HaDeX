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
#' @param confidence_level_2 second confidence level for the test, 
#' from range [0, 1]. If the value of second confidence level is the same
#' as first, only one is shown. 
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
                              confidence_level = 0.98,
                              confidence_level_2 = 0.99){
  
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

  h_interval_2 <- calculate_confidence_limit_values(diff_uptake_dat = diff_uptake_dat,
                                                    confidence_level = confidence_level_2,
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
                                                        value < h_interval_2[1] ~ "deepskyblue3",
                                                        value < h_interval[1] ~ "deepskyblue1",
                                                        value > h_interval_2[2] ~ "firebrick3",
                                                        value > h_interval[2] ~ "firebrick1",
                                                        TRUE ~ "azure3")) %>%
      ggplot() +
      geom_segment(aes(x = Start, y = value, xend = End, yend = value, color = colour)) +
      geom_errorbar(aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = colour)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
      ## intervals
      geom_hline(aes(yintercept = h_interval[1], linetype = paste0(" Confidence interval ", confidence_level*100, "% : ", round(h_interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
      geom_hline(aes(yintercept = h_interval[2], linetype = paste0(" Confidence interval ", confidence_level*100, "% : ", round(h_interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
      geom_hline(aes(yintercept = h_interval_2[1], linetype = paste0(" Confidence interval ", confidence_level_2*100, "% : ", round(h_interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
      geom_hline(aes(yintercept = h_interval_2[2], linetype = paste0(" Confidence interval ", confidence_level_2*100, "% : ", round(h_interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
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

#' Differential butterfly plot
#'
#' @importFrom ggplot2 scale_linetype_manual scale_colour_identity
#'
#' @param diff_uptake_dat data produced by
#' \code{\link{create_diff_uptake_dataset}} function.
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param show_houde_interval \code{logical}, determines if houde interval is shown.
#' @param show_tstud_confidence \code{logical}, determines if t-Student test validity 
#' is shown.
#' @param confidence_level confidence level for the test, from range [0, 1].
#' Important if selected show_confidence_limit.
#' @param uncertainty_type ...
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
#' plot_differential_butterfly(diff_uptake_dat = diff_uptake_dat)
#' 
#' diff_p_uptake_dat <- create_p_diff_uptake_dataset(dat)
#' plot_differential_butterfly(diff_p_uptake_dat = diff_p_uptake_dat, show_tstud_confidence = TRUE)
#' 
#' @export plot_differential_butterfly

plot_differential_butterfly <- function(diff_uptake_dat = NULL,
                                        diff_p_uptake_dat = NULL, 
                                        theoretical = FALSE,
                                        fractional = FALSE,
                                        show_houde_interval = FALSE,
                                        show_tstud_confidence = FALSE,
                                        uncertainty_type = "ribbon",
                                        confidence_level = 0.98){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars", "bars + line"))
  
  ## conditions
  
  if (show_tstud_confidence) {
    
    if(is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } else { diff_uptake_dat <- diff_p_uptake_dat }
    
  } else {
    
    if(is.null(diff_uptake_dat) & is.null(diff_p_uptake_dat)) { stop("Please, provide the neccessary data.") } 
      
  }
  
  ##
  
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
  
  butterfly_differential_plot <- ggplot() +
    geom_point(data = plot_dat, aes(x = ID, y = value, group = Exposure, color = Exposure)) +
    labs(title = title,
         x = "Peptide ID",
         y = y_label) +
    theme(legend.position = "bottom")
  
  if(uncertainty_type == "ribbon"){
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_ribbon(data = plot_dat, aes(x = ID, ymin = value - err_value, ymax = value + err_value, fill = Exposure), alpha = 0.5, size = 0, linetype = "blank")
    
  } else if (uncertainty_type == "bars"){
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_errorbar(data = plot_dat, aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5)
    
  } else if (uncertainty_type == "bars + line"){
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_errorbar(data = plot_dat, aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5) +
      geom_line()
  }
  
  if(show_houde_interval){
    
    t_value <- qt(c((1 - confidence_level)/2, 1-(1 - confidence_level)/2), df = 2)[2]
    x_threshold <- t_value * mean(plot_dat[["err_value"]], na.rm = TRUE)/sqrt(length(plot_dat))
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_hline(aes(yintercept = x_threshold), linetype = "dashed", color = "black", size = .7) +
      geom_hline(aes(yintercept = -x_threshold), linetype = "dashed", color = "black", size = .7) 
    
  }
  
  if(show_tstud_confidence){
    
    alpha <- -log(1 - attr(diff_uptake_dat, "confidence_level"))
    
    diff_uptake_dat <- mutate(diff_uptake_dat, valid = log_p_value >= alpha) %>%
      merge(plot_dat, by = c("Sequence", "Start", "End", "Exposure", "ID"))
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_point(data = subset(diff_uptake_dat, !valid), aes(x = ID, y = value, group = Exposure), color = "grey77", size = 2)
    
  }
  
  # butterfly_differential_plot <- butterfly_differential_plot + labs(color = "Exposure")
  
  return(HaDeXify(butterfly_differential_plot))
  
}

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
  
  plot_dat <- data.frame(ID = diff_uptake_dat[["ID"]],
                         Exposure = as.factor(diff_uptake_dat[["Exposure"]]),
                         value = diff_uptake_dat[[value]],
                         err_value = diff_uptake_dat[[err_value]],
                         Sequence = diff_uptake_dat[["Sequence"]],
                         Start = diff_uptake_dat[["Start"]],
                         End = diff_uptake_dat[["End"]])
  
  chiclet_differential_plot <- ggplot(plot_dat, aes(y = Exposure, x = ID)) +
    geom_tile(aes(fill = value)) +
    geom_tile(data = subset(plot_dat, is.na(value)), fill = "gray95") +
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
  
  if(show_houde_interval){
    
    t_value <- qt(c((1 - confidence_level)/2, 1-(1 - confidence_level)/2), df = 2)[2]
    x_threshold <- t_value * mean(plot_dat[["err_value"]], na.rm = TRUE)/sqrt(length(plot_dat))
    
    chiclet_differential_plot <- chiclet_differential_plot +
      geom_tile(data = subset(plot_dat, abs(value) < x_threshold), fill = "azure3")
    
  }
  
  if(show_tstud_confidence){
    
    alpha <- -log(1 - confidence_level)
    
    diff_uptake_dat <- mutate(diff_uptake_dat, valid = log_p_value >= alpha, Exposure = as.factor(Exposure)) %>%
      merge(plot_dat, by = c("Sequence", "Start", "End", "Exposure", "ID"))
    
    chiclet_differential_plot <- chiclet_differential_plot +
      geom_tile(data = subset(diff_uptake_dat, !valid), aes(x = ID, y = Exposure), fill = "grey77")
    
  }
  
  return(HaDeXify(chiclet_differential_plot))
  
}

