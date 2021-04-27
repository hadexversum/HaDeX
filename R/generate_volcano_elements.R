#' generate_volcano_dataset
#' 
#' Generates data set for volcano plot
#' 
#' @importFrom dplyr %>%
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. Default value - first protein name from 
#' the data.
#' @param state_1 biological state for chosen protein. From this state values
#' the second state values are subtracted to get the deuterium uptake difference.
#' Default value - first state found in the data.
#' @param state_2 biological state for chosen protein. This state values are 
#' subtracted from the first state values to get the deuterium uptake diference.
#' Default value - second state found in the data.
#' @param p_adjustment_method method of adjustment P-values for multiple 
#' comparisons. Possible methods: "BH" (Benjamini & Hochberg correction), 
#' "bonferroni" (Bonferroni correction) and "none" (default).
#' @param confidence_level confidence level for the t-test. Default value - 0.98.
#' 
#' @details The volcano plot shows the deuterium uptake difference in time 
#' for the peptides. This function prepares data for the plot.
#' For peptides in all of the time points of measurement (except for minimal
#' and maximal exchange control) the deuterium uptake difference between state_1
#' and state_2 is calculated, with its uncertainty (combined and propagated as
#' described in `Data processing` article). For each peptide in time point the 
#' P-value is calculated using unpaired t-test - the deuterium uptake difference
#' is calculated as the difference of measured masses in a given time point for two 
#' states. The tested hypothesis is that the mean masses for states from the 
#' replicates of the experiment are similar. The P-values indicates if the null
#' hypothesis can be rejected - rejection of the hypothesis means that the 
#' difference between states is statistically significant at provided confidence
#' level. The P-values can be adjusted using the provided method.
#' 
#' @references Hageman, T. S. & Weis, D. D. Reliable Identification of Significant 
#' Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements 
#' Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
#' 
#' @return a data frame with calculated deuterium uptake difference, uncertainty,
#' P-value and -log(P-value) for the peptides from the provided data.
#' 
#' @seealso \code{\link{generate_volcano_plot}}  \code{\link{generate_volcano_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' generate_volcano_dataset(dat,
#'                          protein = "db_CD160", 
#'                          state_1 = "CD160",
#'                          state_2 = "CD160_HVEM",
#'                          p_adjustment_method = "none",
#'                          confidence_level = 0.99)
#' 
#' @export generate_volcano_dataset

generate_volcano_dataset <- function(dat,
                                     protein = unique(dat[["Protein"]])[1],
                                     state_1 = unique(dat[["State"]])[1],
                                     state_2 = unique(dat[["State"]])[2],
                                     p_adjustment_method = "none",
                                     confidence_level = 0.98){
  
  p_adjustment_method <- match.arg(p_adjustment_method, c("none", "BH", "bonferroni"))
  
  dat <- dat[dat[["Protein"]] == protein, ]
  
  proton_mass <- 1.00727647
  
  tmp_dat <- dat %>%
    calculate_exp_masses_per_replicate(.) %>%
    group_by(Sequence, Start, End, State, Exposure) %>%
    summarize(avg_mass = mean(avg_exp_mass),
              err_avg_mass = sd(avg_exp_mass)/sqrt(length(Exposure)),
              masses = list(avg_exp_mass)) %>%
    arrange(Start, End) 
  
  tmp_dat_1 <- tmp_dat %>%
    filter(State == state_1) %>%
    rename(avg_mass_1 = avg_mass,
           err_avg_mass_1 = err_avg_mass, 
           masses_1 = masses) %>%
    ungroup(.) %>%
    select(-State)
  
  tmp_dat_2 <- tmp_dat %>%
    filter(State == state_2) %>%
    rename(avg_mass_2 = avg_mass,
           err_avg_mass_2 = err_avg_mass,
           masses_2 = masses) %>%
    ungroup(.) %>%
    select(-State)
  
  vol_dat <- merge(tmp_dat_1, tmp_dat_2, by = c("Sequence", "Start", "End", "Exposure"))
  
  res_volcano <- lapply(1:nrow(vol_dat), function(i){
    
    diff_d <- vol_dat[i, "avg_mass_1"] - vol_dat[i, "avg_mass_2"]
    uncertainty <- sqrt(vol_dat[i, "err_avg_mass_1"]^2 + vol_dat[i, "err_avg_mass_2"]^2 )
    
    st_1 <- vol_dat[i, "masses_1"][[1]]
    st_2 <- vol_dat[i, "masses_2"][[1]]
    
    if(length(st_1) == 1) {
      p_value <- -1
    } else if (length(st_2) == 1){
      p_value <- -1
    } else {
      p_value <- t.test(x = st_1, y = st_2, paired = FALSE, alternative = "two.sided", conf.level = confidence_level)$p.value
    }
    
    data.frame(Sequence = vol_dat[i, "Sequence"],
               Exposure = vol_dat[i, "Exposure"],
               D_diff = diff_d,
               P_value = p_value,
               Uncertainty = uncertainty,
               Start = vol_dat[i, "Start"],
               End = vol_dat[i, "End"])
    
  }) %>% bind_rows() %>%
    filter(P_value > 0)
  
  res_volcano[["P_value"]] <- p.adjust(res_volcano[["P_value"]], method = p_adjustment_method)
  
  res_volcano %>%
    filter(P_value!=-1) %>%
    mutate(log_p_value = -log(P_value)) %>%
    select(Sequence, Start, End, Exposure, D_diff, Uncertainty, log_p_value)
  
}

#' generate_volcano_plot
#' 
#' Generates volcano plot based on supplied volcano data
#' 
#' @importFrom ggplot2 coord_cartesian
#' 
#' @param vol_data data produced by the \code{\link{generate_volcano_dataset}} 
#' function.
#' @param state_1 biological state for chosen protein. It is used in the title.
#' Default value - "".
#' @param state_2 biological state for chosen protein. It is used in the title.
#' Default value - "".
#' @param adjust_axes logical, indicating if the X-axis is symmetrical in 
#' relation to 0. Default value - TRUE.
#' @param show_confidence_limits logical, indicates if the hybrid testing 
#' confidence intervals are shown. Default value - FALSE.
#' @param confidence_level confidence level for the confidence intervals. It 
#' should be the same as used in \code{\link{generate_volcano_dataset}} function.
#' Default value - 0.98.
#' 
#' @details The data produced by \code{\link{generate_volcano_dataset}} are plotted
#' in the form of a volcano plot. The generation of the data is described in the documentation
#' of \code{\link{generate_volcano_dataset}} function. The confidence limit on 
#' P-value is calculated based on the confidence level. The confidence limit on deuterium 
#' uptake difference is calculated using the Houde test for the time point of measurement
#' from the provided data. The confidence limits are indicated by the red dotted
#' lines. The points above confidence limits (upper right and left corner) are 
#' statistically significant in hybrid testing. 
#' This plot is visible in GUI. 
#' 
#' @references Hageman, T. S. & Weis, D. D. Reliable Identification of Significant 
#' Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements 
#' Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
#' @references Houde, D., Berkowitz, S.A., and Engen, J.R. (2011). 
#' The Utility of Hydrogen/Deuterium Exchange Mass Spectrometry in 
#' Biopharmaceutical Comparability Studies. J Pharm Sci 100, 2071–2086.
#' 
#' @return a ggplot object.
#' 
#' @seealso \code{\link{generate_volcano_dataset}} \code{\link{generate_volcano_data}} 
#' 
#' @examples 
#' dat <-  dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' vol_dat <- generate_volcano_dataset(dat)
#' generate_volcano_plot(vol_dat, show_confidence_limits = T)
#' 
#' @export generate_volcano_plot

generate_volcano_plot <- function(vol_data, 
                                  state_1 = "", 
                                  state_2 = "",
                                  adjust_axes = TRUE,
                                  show_confidence_limits = FALSE,
                                  confidence_level = 0.98) {
  
  volcano_plot <- ggplot(vol_data, aes(x = D_diff, y = log_p_value)) + 
    geom_point() + 
    geom_errorbar(aes(xmin = D_diff - Uncertainty, xmax = D_diff + Uncertainty), alpha = 0.2) + 
    labs(title = paste0("Volcano Plot ", state_1, " " , state_2),  
         x = "Mass difference [Da]",
         y = "-log(P value)")
  
  if(adjust_axes){
    
    x_max <- ceiling(max(abs(vol_data[["D_diff"]])))
    y_max <- ceiling(max(vol_data[["log_p_value"]])) + 2
    
    volcano_plot <- volcano_plot + 
      coord_cartesian(xlim = c(-x_max, x_max), ylim = c(0, y_max), expand = FALSE) 
    
  }
  
  if(show_confidence_limits){
    
    y_threshold <- -log(1 - confidence_level)
    
    t_value <- qt(c((1 - confidence_level)/2, 1-(1 - confidence_level)/2), df = 2)[2]
    x_threshold <- t_value * mean(vol_data[["Uncertainty"]], na.rm = TRUE)/sqrt(length(vol_dat))
    
    volcano_plot <- volcano_plot + 
      geom_segment(aes(x = -x_threshold, xend = -x_threshold, y = y_threshold, yend = Inf), linetype = "dashed", color = "red") +
      geom_segment(aes(x = x_threshold, xend = x_threshold, y = y_threshold, yend = Inf), linetype = "dashed", color = "red") +
      geom_segment(aes(y = y_threshold, yend = y_threshold, x = -Inf, xend = -x_threshold), linetype = "dashed", color = "red") +
      geom_segment(aes(y = y_threshold, yend = y_threshold, x = x_threshold, xend = Inf), linetype = "dashed", color = "red") 
    
  }
  
  return(volcano_plot)
}

#' generate_volcano_data
#'
#' Generates a nice data frame 
#'  
#' @param vol_data data produced by the \code{\link{generate_volcano_dataset}} 
#' function.
#'
#' @details This function rounds the numerical values (4 places) and 
#' changes the column names to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @return a \code{data.frame} object.
#'
#' @seealso \code{\link{generate_volcano_dataset}} \code{\link{generate_volcano_plot}} 
#' 
#' @examples 
#' dat <-  dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' vol_dat <- generate_volcano_dataset(dat)
#' generate_volcano_data(vol_dat, show_confidence_limits = T)
#' 
#' @export generate_volcano_data

generate_volcano_data <- function(vol_data){
  
  vol_data %>%
    mutate(D_diff  = round(D_diff , 4),
           Uncertainty = round(Uncertainty, 4),
           log_p_value = round(log_p_value, 4)) %>%
    arrange(Exposure, Start, End) %>%
    rename("Deuterium uptake difference" = D_diff , 
           "-log(P value)" = log_p_value)
  
}
