#' Show uptake data
#' 
#' @param uptake_dat data produced by \code{\link{create_uptake_dataset}} 
#' function or \code{\link{create_state_uptake_dataset}}.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param renamed \code{logical}, determines if the names of the columns
#' are renamed to user-friendly ones. Currently FALSE not implemented.
#' 
#' @details This function subsets the dataset based on provided criteria,
#' rounds the numerical values (4 places) and changes the column names 
#' to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @importFrom dplyr rename %>% cur_group_id
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{create_uptake_dataset}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' uptake_dat <- create_uptake_dataset(dat)
#' show_uptake_data(uptake_dat)
#' 
#' @export show_uptake_data

show_uptake_data <- function(uptake_dat,
                             theoretical = FALSE, 
                             fractional = FALSE,
                             renamed = TRUE){
  
  
  if (theoretical){
    
    if (fractional){
      
      uptake_dat %>%
        select(Protein, Sequence, State, Start, End, Exposure, theo_frac_deut_uptake, err_theo_frac_deut_uptake) %>%
        mutate(theo_frac_deut_uptake  = round(theo_frac_deut_uptake , 4),
               err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        group_by(Start, End, Sequence) %>%
        mutate(ID = cur_group_id()) %>%
        ungroup(.) %>%
        rename("Theo Frac DU [%]" = theo_frac_deut_uptake , 
               "U(Theo Frac DU) [%]" = err_theo_frac_deut_uptake) %>%
        select(Protein, Sequence, ID, everything())
      
    } else {
      
      uptake_dat %>%
        select(Protein, Sequence, State, Start, End, Exposure, theo_deut_uptake, err_theo_deut_uptake) %>%
        mutate(theo_deut_uptake = round(theo_deut_uptake, 4),
               err_theo_deut_uptake = round(err_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        group_by(Start, End, Sequence) %>%
        ungroup(.) %>%
        mutate(ID = cur_group_id()) %>%
        rename("Theo DU [Da]" = theo_deut_uptake,
               "U(Theo DU) [Da]" = err_theo_deut_uptake) %>%
        select(Protein, Sequence, ID, everything())
    }
    
  } else {
    
    if (fractional){
      
      uptake_dat %>%
        select(Protein, Sequence, State, Start, End, Exposure, frac_deut_uptake, err_frac_deut_uptake) %>%
        mutate(frac_deut_uptake = round(frac_deut_uptake, 4),
               err_frac_deut_uptake = round(err_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        group_by(Start, End, Sequence) %>%
        mutate(ID = cur_group_id()) %>%
        ungroup(.) %>%
        rename("Frac DU [%]" = frac_deut_uptake,
               "U(Frac DU) [%]" = err_frac_deut_uptake) %>%
        select(Protein, Sequence, ID, everything())
      
    } else {
      
      uptake_dat %>%
        select(Protein, Sequence, State, Start, End, Exposure, deut_uptake, err_deut_uptake) %>%
        mutate(deut_uptake = round(deut_uptake, 4),
               err_deut_uptake = round(err_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        group_by(Start, End, Sequence) %>%
        mutate(ID = cur_group_id()) %>%
        ungroup(.) %>%
        rename("DU [Da]" = deut_uptake,
               "U(DU) [Da]" = err_deut_uptake) %>%
        select(Protein, Sequence, ID, everything())
      
    }
    
  }
  
}



#' Show differential uptake data
#' 
#' @param diff_uptake_dat data produced by 
#' \code{\link{create_diff_uptake_dataset}} function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param renamed \code{logical}, determines if the names of the columns
#' are renamed to user-friendly ones. Currently FALSE not implemented.
#' 
#' @details This function subsets the dataset based on provided criteria,
#' rounds the numerical values (4 places) and changes the column names 
#' to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{create_diff_uptake_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' head(show_diff_uptake_data(diff_uptake_dat))
#' 
#' @export show_diff_uptake_data

show_diff_uptake_data <- function(diff_uptake_dat, 
                                  theoretical = FALSE, 
                                  fractional = FALSE,
                                  renamed = TRUE){
  
  if(theoretical){
    
    if(fractional){
      
      diff_uptake_dat %>%
        select(Protein, Sequence, Start, End, Exposure, diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake) %>%
        mutate(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
               err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        group_by(Start, End, Sequence) %>%
        mutate(ID = cur_group_id()) %>%
        rename("Theo Frac Diff DU [%]" = diff_theo_frac_deut_uptake,
               "U(Theo Frac Diff DU) [%]" = err_diff_theo_frac_deut_uptake) %>%
        select(Protein, ID, everything())
      
    } else {
      
      diff_uptake_dat %>%
        select(Protein, Sequence, Start, End, Exposure, diff_theo_deut_uptake, err_diff_theo_deut_uptake) %>%
        mutate(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
               err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        group_by(Start, End, Sequence) %>%
        mutate(ID = cur_group_id()) %>%
        rename("Theo Diff DU [Da]" = diff_theo_deut_uptake,
               "U(Theo Diff DU) [Da]" = err_diff_theo_deut_uptake) %>%
        select(Protein, ID, everything())
      
    }
    
  } else {
    
    if(fractional){
      
      diff_uptake_dat %>%
        select(Protein, Sequence, Start, End, Exposure, diff_frac_deut_uptake, err_diff_frac_deut_uptake) %>%
        mutate(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
               err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        group_by(Start, End, Sequence) %>%
        mutate(ID = cur_group_id()) %>%
        rename("Frac Diff DU [%]" = diff_frac_deut_uptake,
               "U(Frac Diff DU) [%]" = err_diff_frac_deut_uptake) %>%
        select(Protein, ID, everything())
      
    } else {
      
      diff_uptake_dat %>%
        select(Protein, Sequence, Start, End, Exposure, diff_deut_uptake, err_diff_deut_uptake) %>%
        mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
               err_diff_deut_uptake = round(err_diff_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        group_by(Start, End, Sequence) %>%
        mutate(ID = cur_group_id()) %>%
        rename("Diff DU [Da]" = diff_deut_uptake,
               "U(Diff DU) [Da]" = err_diff_deut_uptake) %>%
        select(Protein, ID, everything())
      
    }
    
  }
  
}

#' Show differential uptake data with confidence levels 
#' 
#' @param diff_uptake_dat data produced by \code{\link{create_diff_uptake_dataset}}
#' function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param confidence_level_1 confidence level for the test, from range [0, 1].
#' @param confidence_level_2 second confidence level for the test, 
#' from range [0, 1]. If the value of second confidence level is the same
#' as first, only one is shown. 
#' 
#' @details This function subsets the dataset based on provided criteria,
#' rounds the numerical values (4 places) and changes the column names 
#' to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{create_diff_uptake_dataset}}
#' \code{\link{plot_differential}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' show_diff_uptake_data_confidence(diff_uptake_dat)
#' 
#' @export show_diff_uptake_data_confidence

show_diff_uptake_data_confidence <- function(diff_uptake_dat, 
                                             theoretical = FALSE, 
                                             fractional = FALSE,
                                             confidence_level_1 = 0.98,
                                             confidence_level_2 = 0.99){
  
  column_name_cl1 <- paste0("Valid At ", confidence_level_1)
  column_name_cl2 <- paste0("Valid At ", confidence_level_2)
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional  
      diff_uptake_dat %>%
        add_stat_dependency(confidence_level = confidence_level_1,
                            theoretical = TRUE, 
                            fractional = TRUE) %>%
        add_stat_dependency(confidence_level = confidence_level_2,
                            theoretical = TRUE, 
                            fractional = TRUE) %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake, paste0("valid_at_", confidence_level_1), paste0("valid_at_", confidence_level_2)) %>%
        mutate(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
               err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Frac Diff DU [%]" = diff_theo_frac_deut_uptake,
               "U(Theo Frac Diff DU) [%]" = err_diff_theo_frac_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_level_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_level_2))
      
    } else {
      # theoretical & absolute
      diff_uptake_dat %>%
        add_stat_dependency(confidence_level = confidence_level_1,
                            theoretical = TRUE, 
                            fractional = FALSE) %>%
        add_stat_dependency(confidence_level = confidence_level_2,
                            theoretical = TRUE, 
                            fractional = FALSE) %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_deut_uptake, err_diff_theo_deut_uptake, paste0("valid_at_", confidence_level_1), paste0("valid_at_", confidence_level_2)) %>%
        mutate(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
               err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Diff DU [Da]" = diff_theo_deut_uptake,
               "U(Theo Diff DU) [Da]" = err_diff_theo_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_level_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_level_2))
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      diff_uptake_dat %>%
        add_stat_dependency(confidence_level = confidence_level_1,
                            theoretical = FALSE, 
                            fractional = TRUE) %>%
        add_stat_dependency(confidence_level = confidence_level_2,
                            theoretical = FALSE, 
                            fractional = TRUE) %>%
        select(Protein, Sequence,ID, Start, End, Exposure, diff_frac_deut_uptake, err_diff_frac_deut_uptake, paste0("valid_at_", confidence_level_1), paste0("valid_at_", confidence_level_2)) %>%
        mutate(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
               err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Frac Diff DU [%]" = diff_frac_deut_uptake,
               "U(Frac Diff DU) [%]" = err_diff_frac_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_level_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_level_2))
      
    } else {
      # experimental & absolute
      diff_uptake_dat %>%
        add_stat_dependency(confidence_level = confidence_level_1,
                            theoretical = FALSE,
                            fractional = FALSE) %>%
        add_stat_dependency(confidence_level = confidence_level_2,
                            theoretical = FALSE, 
                            fractional = FALSE) %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_deut_uptake, err_diff_deut_uptake, paste0("valid_at_", confidence_level_1), paste0("valid_at_", confidence_level_2)) %>%
        mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
               err_diff_deut_uptake = round(err_diff_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff DU [Da]" = diff_deut_uptake,
               "U(Diff DU) [Da]" = err_diff_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_level_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_level_2))
    }
  }
  
  
}



#' Show volcano data 
#'  
#' @param p_dat data produced by the \code{\link{create_p_diff_uptake_dataset}} 
#' function.
#' @param D_diff_threshold \code{numeric}, threshold on differential deuterium 
#' uptake, e.q. result of Houde test. If not provided, is calculated as 
#' Houde test at provided confidence level. 
#' @param log_P_threshold \code{numeric}, threshold on P-value (after transformation,
#' -log(P)). If not provided, is calculated at provided confidence level. 
#' @param confidence_level \code{numeric}, confidence level. 
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#'
#' @details This function subsets the dataset based on provided criteria,
#' rounds the numerical values (4 places) and changes the column names 
#' to user-friendly ones. It adds the column `valid at confidence_level` to
#' indicate if the measurement is valid under hybrid testing.
#' This data is available in the GUI. 
#' 
#' @references Hageman, T. S. & Weis, D. D. Reliable Identification of Significant 
#' Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements 
#' Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
#' @references Houde, D., Berkowitz, S.A., and Engen, J.R. (2011). 
#' The Utility of Hydrogen/Deuterium Exchange Mass Spectrometry in 
#' Biopharmaceutical Comparability Studies. J Pharm Sci 100, 2071–2086.
#' 
#' @return a \code{\link{data.frame}} object.
#'
#' @seealso 
#' \code{\link{create_p_diff_uptake_dataset}} 
#' \code{\link{plot_volcano}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' vol_dat <- create_p_diff_uptake_dataset(dat)
#' head(show_volcano_data(vol_dat))
#' 
#' @export show_volcano_data

show_volcano_data <- function(p_dat,
                              D_diff_threshold = NULL,
                              log_P_threshold = NULL,
                              confidence_level = 0.98,
                              theoretical = FALSE, 
                              fractional = FALSE){
  
  if(is.null(log_P_threshold)){
    
    log_P_threshold <- -log(1 - confidence_level)
    
  }
  
  if (fractional){
    
    if(theoretical){
      value <- "diff_theo_frac_deut_uptake"
      err_value <- "err_diff_theo_frac_deut_uptake"

    } else {
      value <- "diff_frac_deut_uptake"
      err_value <- "err_diff_frac_deut_uptake" 
    }
    
  } else {
    
    if(theoretical){
      value <- "diff_theo_deut_uptake"
      err_value <- "err_diff_theo_deut_uptake"
      
    } else {
      value <- "diff_deut_uptake"
      err_value <- "err_diff_deut_uptake"
      
    }
  }
  
  if(is.null(D_diff_threshold)){
    
    t_value <- qt(c((1 - confidence_level)/2, 1-(1 - confidence_level)/2), df = 2)[2]
    D_diff_threshold <-  t_value * mean(p_dat[[err_value]], na.rm = TRUE)/sqrt(length(p_dat))
    
  }
  
  plot_dat <- data.frame(Protein = p_dat[["Protein"]],
                         ID = p_dat[["ID"]],
                         Sequence = p_dat[["Sequence"]],
                         Start = p_dat[["Start"]],
                         End = p_dat[["End"]],
                         Exposure = as.factor(p_dat[["Exposure"]]),
                         value = round(p_dat[[value]], 4),
                         err_value = round(p_dat[[err_value]], 4),
                         p_value = round(p_dat[["P_value"]], 4),
                         log_p_value = round(p_dat[["log_p_value"]], 4), 
                         Valid = (abs(p_dat[[value]]) >= D_diff_threshold & p_dat[["log_p_value"]] >= log_P_threshold))
  
}

#' Show summary data
#' 
#' @description Generates summary table.
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param confidence_limit_1 confidence level for the test, from range [0, 1].
#' Used in the calculations.
#' @param confidence_limit_2 second confidence level for the test, 
#' from range [0, 1]. Used in the calculations.
#' 
#' @details The format in the table is suggested in Masson, G.R., Burke, J.E., 
#' Ahn, N.G., Anand, G.S., Borchers, C., Brier, S., Bou-Assaf, G.M., Engen, J.R., 
#' Englander, S.W., Faber, J., et al. (2019). Recommendations for performing, 
#' interpreting and reporting hydrogen deuterium exchange mass spectrometry 
#' (HDX-MS) experiments. Nat Methods 16, 595–602
#' 
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' @export show_summary_data

show_summary_data <- function(dat, 
                              confidence_limit_1,
                              confidence_limit_2,
                              overlap_distribution_data, 
                              protein_length = NULL){
  
  n_reps <- group_by(dat, Protein, Start, End, Sequence, State, Exposure) %>%
    summarise(n_rep = length(unique(File))) %>%
    ungroup() %>% 
    pull(n_rep) %>% 
    table() %>% 
    sort(decreasing = TRUE) %>% 
    names() %>% 
    as.numeric()
  
  data.frame(Name = c("HDX time course", 
                      "Number of peptides",
                      "Sequence coverage ",
                      "Average peptide length",
                      "Redundancy",
                      "Replicates",
                      #"Average standard deviation",
                      "Significant differences in HDX"), 
             Value = c(sum(unique(dat[["Exposure"]]) > 0 ),  
                       length(unique(dat[["Sequence"]])), 
                       paste0(get_protein_coverage(dat, protein_length), "%"),
                       round(mean(nchar(unique(dat[["Sequence"]]))), 2),# average peptide length
                       get_protein_redundancy(dat, protein_length),
                       n_reps[1], 
                       #NA, 
                       paste0(confidence_limit_1, " | ", confidence_limit_2))
  )
  
}