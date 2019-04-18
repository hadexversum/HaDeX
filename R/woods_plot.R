#' Plot Woods' plot
#' 
#' Produces Woods' plot based on theoretical or experimental HDX-MS data. 
#' 
#' @importFrom ggplot2 ggplot scale_linetype_manual scale_colour_identity
#' 
#' @param calc_dat data as imported by the \code{\link{read_hdx}} function and processed by the \code{\link{prepare_dataset}} function.
#' @param theoretical \code{logical}, determines if plot shows theoretical values. 
#' @param relative \code{logical}, determines if values are relative or absolute. 
#' @param confidence_limit confidence limit.
#' @param confidence_limit_2 confidence limit 2.
#' 
#' @references Woods, V.L., and Hamuro, Y. (2001). High resolution, 
#' high-throughput amide deuterium exchange-mass spectrometry (DXMS) 
#' determination of protein binding site structure and dynamics: utility 
#' in pharmaceutical design. J. Cell. Biochem. Suppl. Suppl 37, 89–98.
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' calc_dat <- prepare_dataset(dat,
#'                             in_state_first = "CD160_0.001",
#'                             chosen_state_first = "CD160_1",
#'                             out_state_first = "CD160_1440",
#'                             in_state_second = "CD160_HVEM_0.001",
#'                             chosen_state_second = "CD160_HVEM_1",
#'                             out_state_second = "CD160_HVEM_1440") 
#' woods_plot(calc_dat = calc_dat,
#'            theoretical = TRUE,
#'            relative = TRUE,
#'            confidence_limit = 0.98,
#'            confidence_limit_2 = 0.99)
#' woods_plot(calc_dat = calc_dat,
#'            theoretical = FALSE,
#'            relative = TRUE,
#'            confidence_limit = 0.98,
#'            confidence_limit_2 = 0.99)
#' woods_plot(calc_dat = calc_dat,
#'            theoretical = TRUE,
#'            relative = FALSE,
#'            confidence_limit = 0.98,
#'            confidence_limit_2 = 0.99)
#' woods_plot(calc_dat = calc_dat,
#'            theoretical = FALSE,
#'            relative = FALSE,
#'            confidence_limit = 0.98,
#'            confidence_limit_2 = 0.99)
#'             
#' @export woods_plot

woods_plot <- function(calc_dat,
                       theoretical = FALSE,
                       relative = TRUE,
                       confidence_limit = 0.98,
                       confidence_limit_2 = 0.99){
  if (relative) {
    
    relative_woods_plot(calc_dat = calc_dat,
                        theoretical = theoretical,
                        confidence_limit = confidence_limit,
                        confidence_limit_2 = confidence_limit_2)
    
  } else {
    
    absolute_woods_plot(calc_dat = calc_dat,
                        theoretical = theoretical,
                        confidence_limit = confidence_limit,
                        confidence_limit_2 = confidence_limit_2)
    
  }
  
}

absolute_woods_plot <- function(calc_dat,
                                theoretical = FALSE,
                                confidence_limit = 0.98,
                                confidence_limit_2 = 0.99){
  
  if (theoretical){
    
    interval <- calculate_confidence_limit_values(calc_dat = calc_dat,
                                                  confidence_limit = confidence_limit,
                                                  theoretical = TRUE,
                                                  relative = FALSE)
    
    interval_2 <- calculate_confidence_limit_values(calc_dat = calc_dat,
                                                    confidence_limit = confidence_limit_2,
                                                    theoretical = TRUE,
                                                    relative = FALSE)
    
    calc_dat[["colour"]] <- case_when(
      calc_dat[["abs_diff_theo_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
      calc_dat[["abs_diff_theo_frac_exch"]] < interval[1] ~ "deepskyblue1",
      calc_dat[["abs_diff_theo_frac_exch"]] > interval_2[2] ~ "firebrick3",
      calc_dat[["abs_diff_theo_frac_exch"]] > interval[2] ~ "firebrick1",
      TRUE ~ "azure4"
    )
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = abs_diff_theo_frac_exch, xend = End, yend = abs_diff_theo_frac_exch, color = colour)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_diff_theo_frac_exch - err_abs_diff_theo_frac_exch, ymax = abs_diff_theo_frac_exch + err_abs_diff_theo_frac_exch, color = colour)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
      geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
      geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
      geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
      geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
      scale_linetype_manual(values = c("dashed", "dotdash")) + 
      scale_colour_identity() +
      scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "vertical") +
      labs(x = "Position in sequence", 
           y = expression(paste(Delta, " theoretical absolute value exchanged [Da]")), 
           title = expression(paste(Delta, " Theoretical absoute value exchanged between states in chosen time")))
    
  } else {
    
    interval <- calculate_confidence_limit_values(calc_dat = calc_dat,
                                                  confidence_limit = confidence_limit,
                                                  theoretical = FALSE,
                                                  relative = FALSE)
    
    interval_2 <- calculate_confidence_limit_values(calc_dat = calc_dat,
                                                    confidence_limit = confidence_limit_2,
                                                    theoretical = FALSE,
                                                    relative = FALSE)
    
    calc_dat[["colour"]] <- case_when(
      calc_dat[["abs_diff_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
      calc_dat[["abs_diff_frac_exch"]] < interval[1] ~ "deepskyblue1",
      calc_dat[["abs_diff_frac_exch"]] > interval_2[2] ~ "firebrick3",
      calc_dat[["abs_diff_frac_exch"]] > interval[2] ~ "firebrick1",
      TRUE ~ "azure4"
    )
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = abs_diff_frac_exch, xend = End, yend = abs_diff_frac_exch, color = colour)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = abs_diff_frac_exch - err_abs_diff_frac_exch, ymax = abs_diff_frac_exch + err_abs_diff_frac_exch, color = colour)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
      geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
      geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
      geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
      geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
      scale_linetype_manual(values = c("dashed", "dotdash")) + 
      scale_colour_identity() +
      scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "vertical") +
      labs(x = "Position in sequence", 
           y = expression(paste(Delta, " absolute value exchanged [Da]")), 
           title = expression(paste(Delta, " Absolute value exchanged between states in chosen time")))
    
  }
  
}

relative_woods_plot <- function(calc_dat,
                                theoretical = FALSE,
                                confidence_limit = 0.98,
                                confidence_limit_2 = 0.99){
  
  if (theoretical){
    
    interval <- calculate_confidence_limit_values(calc_dat = calc_dat,
                                                  confidence_limit = confidence_limit,
                                                  theoretical = TRUE,
                                                  relative = TRUE)
    
    interval_2 <- calculate_confidence_limit_values(calc_dat = calc_dat,
                                                    confidence_limit = confidence_limit_2,
                                                    theoretical = TRUE,
                                                    relative = TRUE)
    
    calc_dat[["colour"]] <- case_when(
      calc_dat[["diff_theo_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
      calc_dat[["diff_theo_frac_exch"]] < interval[1] ~ "deepskyblue1",
      calc_dat[["diff_theo_frac_exch"]] > interval_2[2] ~ "firebrick3",
      calc_dat[["diff_theo_frac_exch"]] > interval[2] ~ "firebrick1",
      TRUE ~ "azure4"
    )
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = diff_theo_frac_exch, xend = End, yend = diff_theo_frac_exch, color = colour)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = diff_theo_frac_exch - err_diff_theo_frac_exch, ymax = diff_theo_frac_exch + err_diff_theo_frac_exch, color = colour)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
      geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
      geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
      geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
      geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
      scale_linetype_manual(values = c("dashed", "dotdash")) + 
      scale_colour_identity() +
      scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "vertical") +
      labs(x = "Position in sequence", 
           y = expression(paste(Delta, " theoretical fraction exchanged [%]")), 
           title = expression(paste(Delta, " Theoretical fraction exchanged between states in chosen time")))
    
  } else {
    
    interval <- calculate_confidence_limit_values(calc_dat = calc_dat,
                                                  confidence_limit = confidence_limit,
                                                  theoretical = FALSE,
                                                  relative = TRUE)
    
    interval_2 <- calculate_confidence_limit_values(calc_dat = calc_dat,
                                                    confidence_limit = confidence_limit_2,
                                                    theoretical = FALSE,
                                                    relative = TRUE)
    
    calc_dat[["colour"]] <- case_when(
      calc_dat[["diff_frac_exch"]] < interval_2[1] ~ "deepskyblue3",
      calc_dat[["diff_frac_exch"]] < interval[1] ~ "deepskyblue1",
      calc_dat[["diff_frac_exch"]] > interval_2[2] ~ "firebrick3",
      calc_dat[["diff_frac_exch"]] > interval[2] ~ "firebrick1",
      TRUE ~ "azure4"
    )
    
    ggplot() +
      geom_segment(data = calc_dat, aes(x = Start, y = diff_frac_exch, xend = End, yend = diff_frac_exch, color = colour)) +
      geom_errorbar(data = calc_dat, aes(x = Med_Sequence, ymin = diff_frac_exch - err_frac_exch, ymax = diff_frac_exch + err_frac_exch, color = colour)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
      geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
      geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
      geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
      geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
      scale_linetype_manual(values = c("dashed", "dotdash")) + 
      scale_colour_identity() +
      scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
      theme(legend.title = element_blank(),
            legend.position = "bottom",
            legend.direction = "vertical") +
      labs(x = "Position in sequence",
           y = expression(paste(Delta, " fraction exchanged [%]")), 
           title = expression(paste(Delta, " Fraction exchanged between states in chosen time")))
      
    
  }
  
}
