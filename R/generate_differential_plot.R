#' generate_differential_plot
#' 
#' @description Generates differential (Woods) plot with confidence values
#' based on supplied data and parameters.
#' 
#' @param dat produced by \code{\link{generate_differential_data_set}} function
#' @param theoretical ...
#' @param relative ...
#' @param confidence_limit ...
#' @param confidence_limit_2 ...
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_differential_plot

generate_differential_plot <- function(dat, 
                                       theoretical, 
                                       relative,
                                       confidence_limit, 
                                       confidence_limit_2){ 
  
  if(theoretical){
    
    if(relative){
      # theoretical & relative  
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = TRUE,
                                                    relative = TRUE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = TRUE,
                                                      relative = TRUE)
      
      mutate(dat, colour = case_when(
        dat[["diff_theo_frac_deut_uptake"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_theo_frac_deut_uptake"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_theo_frac_deut_uptake"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_theo_frac_deut_uptake"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_theo_frac_deut_uptake, xend = End, yend = diff_theo_frac_deut_uptake, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_theo_frac_deut_uptake - err_diff_theo_frac_deut_uptake, ymax = diff_theo_frac_deut_uptake + err_diff_theo_frac_deut_uptake, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) + 
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-100, 100)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
      
    } else {
      # theoretical & absolute
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = TRUE,
                                                    relative = FALSE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = TRUE,
                                                      relative = FALSE)
      
      mutate(dat, colour = case_when(
        dat[["diff_theo_deut_uptake"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_theo_deut_uptake"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_theo_deut_uptake"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_theo_deut_uptake"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_theo_deut_uptake, xend = End, yend = diff_theo_deut_uptake, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_theo_deut_uptake - err_diff_theo_deut_uptake, ymax = diff_theo_deut_uptake + err_diff_theo_deut_uptake, color = colour)) +
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
              legend.direction = "vertical") 
    }
    
  } else {
    
    if(relative){
      # experimental & relative
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = FALSE,
                                                    relative = TRUE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = FALSE,
                                                      relative = TRUE)
      
      mutate(dat, colour = case_when(
        dat[["diff_frac_deut_uptake"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_frac_deut_uptake"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_frac_deut_uptake"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_frac_deut_uptake"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_frac_deut_uptake, xend = End, yend = diff_frac_deut_uptake, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_frac_deut_uptake - err_diff_frac_deut_uptake, ymax = diff_frac_deut_uptake + err_diff_frac_deut_uptake, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) +
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-100, 100)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
      
    } else {
      # experimental & absolute
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = FALSE,
                                                    relative = FALSE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = FALSE,
                                                      relative = FALSE)
      
      mutate(dat, colour = case_when(
        dat[["diff_deut_uptake"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_deut_uptake"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_deut_uptake"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_deut_uptake"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_deut_uptake, xend = End, yend = diff_deut_uptake, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_deut_uptake - err_diff_deut_uptake, ymax = diff_deut_uptake + err_diff_deut_uptake, color = colour)) +
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
              legend.direction = "vertical") 
      
    }
  }
  
}