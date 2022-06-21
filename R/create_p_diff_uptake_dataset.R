#' Create differential uptake dataset with p-value 
#'  
#' @importFrom dplyr %>%
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state_1 biological state for chosen protein. From this state values
#' the second state values are subtracted to get the deuterium uptake difference.
#' @param state_2 biological state for chosen protein. This state values are 
#' subtracted from the first state values to get the deuterium uptake difference.
#' @param p_adjustment_method method of adjustment P-values for multiple 
#' comparisons. Possible methods: "BH" (Benjamini & Hochberg correction), 
#' "bonferroni" (Bonferroni correction) and "none" (default).
#' @param confidence_level confidence level for the t-test.
#' @param time_0 minimal exchange control time point of measurement [min].
#' @param time_100 maximal exchange control time point of measurement [min]. 
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details REWRITE!!
#' The volcano plot shows the deuterium uptake difference in time 
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
#' This plot is visible in GUI.
#' 
#' @references Hageman, T. S. & Weis, D. D. Reliable Identification of Significant 
#' Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements 
#' Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
#' 
#' @return a \code{\link{data.frame}} object with calculated deuterium uptake difference
#' in different forms with their uncertainty, P-value and -log(P-value) for the peptides 
#' from the provided data.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_exp_masses_per_replicate}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' p_diff_uptake_dat <- create_p_diff_uptake_dataset(dat)
#' head(p_diff_uptake_dat)
#' 
#' @export create_p_diff_uptake_dataset

create_p_diff_uptake_dataset <- function(dat,
                                         diff_uptake_dat = NULL, 
                                         protein = unique(dat[["Protein"]])[1],
                                         state_1 = unique(dat[["State"]])[1],
                                         state_2 = unique(dat[["State"]])[2],
                                         p_adjustment_method = "none",
                                         confidence_level = 0.98,
                                         time_0 = min(dat[["Exposure"]]),
                                         time_100 = max(dat[["Exposure"]]),
                                         deut_part = 0.9){
  
  p_dat <- calculate_p_value(dat = dat, 
                             protein = protein,
                             state_1 = state_1,
                             state_2 = state_2, 
                             p_adjustment_method = p_adjustment_method,
                             confidence_level = confidence_level)
  
  if(is.null(diff_uptake_dat)) {
    
    diff_uptake_dat <- create_diff_uptake_dataset(dat = dat, 
                                                  protein = protein,
                                                  state_1 = state_1,
                                                  state_2 = state_2, 
                                                  time_0 = time_0,
                                                  time_100 = time_100, 
                                                  deut_part = deut_part)
  }
  
  
  p_diff_uptake_dat <- merge(diff_uptake_dat, p_dat, by = c("Protein", "Sequence", "Start", "End", "Exposure", "Modification")) %>%
    arrange(Protein, Start, End)
  
  attr(p_diff_uptake_dat, "protein") <- protein
  attr(p_diff_uptake_dat, "state_1") <- state_1
  attr(p_diff_uptake_dat, "state_2") <- state_2
  attr(p_diff_uptake_dat, "confidence_level") <- confidence_level
  attr(p_diff_uptake_dat, "p_adjustment_method") <- p_adjustment_method
  attr(p_diff_uptake_dat, "time_0") <- time_0
  attr(p_diff_uptake_dat, "time_100") <- time_100
  attr(p_diff_uptake_dat, "deut_part") <- deut_part
  attr(p_diff_uptake_dat, "has_modification") <- attr(dat, "has_modification")
  attr(p_diff_uptake_dat, "n_rep") <- attr(dat, "n_rep")
  
  return(p_diff_uptake_dat)
  
}