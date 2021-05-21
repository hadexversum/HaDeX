#' Generate differential dataset
#' 
#' @importFrom tidyr gather
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states vector of two states for chosen protein. Order is important, as the 
#' deuterium uptake difference is calculated as state_1 - state_2.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_t time point of the measurement for which the calculations
#' are done. 
#' @param time_100 maximal exchange control time point of measurement.
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details Function \code{\link{generate_differential_data_set}} calculates
#' differential values based on provided criteria for peptides for chosen
#' protein in selected states. The methods of calculation of deuterium uptake
#' difference, fractional deuterium uptake difference with respect to 
#' minimal/maximal exchange controls or theoretical tabular values are
#' thoroughly described in the `Data processing` article, as well as 
#' law of propagation of uncertainty, used to calculate uncertainty. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_deuteration}}
#' \code{\link{generate_differential_data}}
#' \code{\link{generate_differential_plot}}
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_dat <- generate_differential_data_set(dat)
#' head(diff_dat)
#' 
#' @export generate_differential_data_set

generate_differential_data_set <- function(dat,
                                           protein = unique(dat[["Protein"]][1]),
                                           states = unique(dat[["State"]])[1:2],
                                           time_0 = 0.001,
                                           time_t = 1,
                                           time_100 = 1440,
                                           deut_part = 0.9){
  
  bind_rows(lapply(states, function(i) calculate_state_deuteration(dat, 
                                                                   protein = protein, 
                                                                   state = i, 
                                                                   time_0 = time_0,
                                                                   time_t = time_t, 
                                                                   time_100 = time_100,
                                                                   deut_part = deut_part))) %>%
    droplevels() %>% 
    mutate(State = factor(State, levels = states, labels = c("1", "2"))) %>%
    gather(variable, value, -c(Protein:End, State, Med_Sequence)) %>%
    unite(tmp, variable, State) %>%
    spread(tmp, value)  %>%
    mutate(diff_frac_deut_uptake = frac_deut_uptake_1 - frac_deut_uptake_2,
           err_diff_frac_deut_uptake = sqrt(err_frac_deut_uptake_1^2 + err_frac_deut_uptake_2^2),
           diff_deut_uptake = deut_uptake_1 - deut_uptake_2,
           err_diff_deut_uptake = sqrt(err_deut_uptake_1^2 + err_deut_uptake_2^2),
           diff_theo_frac_deut_uptake = theo_frac_deut_uptake_1 - theo_frac_deut_uptake_2, 
           err_diff_theo_frac_deut_uptake = sqrt(err_theo_frac_deut_uptake_1^2 + err_theo_frac_deut_uptake_2^2),
           diff_theo_deut_uptake = theo_deut_uptake_1 - theo_deut_uptake_2,
           err_diff_theo_deut_uptake = sqrt(err_theo_deut_uptake_1^2 + err_theo_deut_uptake_2^2)) %>%
    arrange(Start, End) %>%
    select(Protein, Start, End, Med_Sequence, everything(), -contains("1"), -contains("2"))
}


#' Generate differential data
#' 
#' @param dat data produced by \code{\link{generate_differential_data_set}}
#' function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param confidence_limit_1 ...
#' @param confidence_limit_2 ...
#' 
#' @details This function subsets the dataset based on provided criteria,
#' rounds the numerical values (4 places) and changes the column names 
#' to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{generate_differential_data_set}}
#' \code{\link{generate_differential_plot}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_dat <- generate_differential_data_set(dat)
#' generate_differential_data(diff_dat)
#' 
#' @export generate_differential_data

generate_differential_data <- function(dat, 
                                       theoretical = FALSE, 
                                       fractional = FALSE,
                                       confidence_limit_1 = 0.98,
                                       confidence_limit_2 = 0.99){
  
  column_name_cl1 <- paste0("Valid At ", confidence_limit_1)
  column_name_cl2 <- paste0("Valid At ", confidence_limit_2)
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional  
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = TRUE, 
                            fractional = TRUE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = TRUE, 
                            fractional = TRUE) %>%
        select(Protein, Sequence, Start, End, diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
               err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Diff Frac Exch" = diff_theo_frac_deut_uptake,
               "Err Theo Diff Frac Exch" = err_diff_theo_frac_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
      
    } else {
      # theoretical & absolute
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = TRUE, 
                            fractional = FALSE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = TRUE, 
                            fractional = FALSE) %>%
        select(Protein, Sequence, Start, End, diff_theo_deut_uptake, err_diff_theo_deut_uptake, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
               err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Abs Value Diff" = diff_theo_deut_uptake,
               "Err Theo Abs Value Diff" = err_diff_theo_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = FALSE, 
                            fractional = TRUE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = FALSE, 
                            fractional = TRUE) %>%
        select(Protein, Sequence, Start, End, diff_frac_deut_uptake, err_diff_frac_deut_uptake, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
               err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Frac Exch" = diff_frac_deut_uptake,
               "Err Diff Frac Exch" = err_diff_frac_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
      
    } else {
      # experimental & absolute
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = FALSE,
                            fractional = FALSE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = FALSE, 
                            fractional = FALSE) %>%
        select(Protein, Sequence, Start, End, diff_deut_uptake, err_diff_deut_uptake, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
               err_diff_deut_uptake = round(err_diff_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Abs Value Exch" = diff_deut_uptake,
               "Err Diff Abs Value Exch" = diff_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
    }
  }
  
  
}


