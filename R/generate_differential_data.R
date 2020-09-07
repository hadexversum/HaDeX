#' generate_differential_data
#' 
#' @description Generates differential data, based on the supplied
#' parameters.
#' 
#' @param dat custom format, produced by 
#' \code{\link{generate_differential_data_set}}
#' @param theoretical ...
#' @param relative ...
#' @param confidence_limit_1 ...
#' @param confidence_limit_2 ...
#' 
#' @details This data is available in the GUI. 
#' All of the numerical values are rounded to 4 places after the dot!!
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_differential_data

generate_differential_data <- function(dat, 
                                       theoretical, 
                                       relative,
                                       confidence_limit_1,
                                       confidence_limit_2){
  
  column_name_cl1 <- paste0("Valid At ", confidence_limit_1)
  column_name_cl2 <- paste0("Valid At ", confidence_limit_2)
  
  if(theoretical){
    
    if(relative){
      # theoretical & relative  
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = TRUE, 
                            relative = TRUE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = TRUE, 
                            relative = TRUE) %>%
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
                            relative = FALSE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = TRUE, 
                            relative = FALSE) %>%
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
    
    if(relative){
      # experimental & relative
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = FALSE, 
                            relative = TRUE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = FALSE, 
                            relative = TRUE) %>%
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
                            relative = FALSE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = FALSE, 
                            relative = FALSE) %>%
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