#' generate_woods_data
#' 
#' @description Generates Woods Plot data, compatible 
#' with the plot. 
#' 
#' @param dat custom format, produced by \code{\link{generate_differential_data}}
#' @param theoretical ...
#' @param relative ...
#' @param confidence_limit_1 ...
#' @param confidence_limit_2 ...
#' 
#' @details This data is accessible from the GUI.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_woods_data

generate_woods_data <- function(dat, 
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
        select(Protein, Sequence, Start, End, diff_theo_frac_exch, err_diff_theo_frac_exch, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_theo_frac_exch = round(diff_theo_frac_exch, 4),
               err_diff_theo_frac_exch = round(err_diff_theo_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Diff Frac Exch" = diff_theo_frac_exch,
               "Err Theo Diff Frac Exch" = err_diff_theo_frac_exch,
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
        select(Protein, Sequence, Start, End, abs_diff_theo_frac_exch, err_abs_diff_theo_frac_exch, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(abs_diff_theo_frac_exch = round(abs_diff_theo_frac_exch, 4),
               err_abs_diff_theo_frac_exch = round(err_abs_diff_theo_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Abs Value Diff" = abs_diff_theo_frac_exch,
               "Err Theo Abs Value Diff" = err_abs_diff_theo_frac_exch,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
    }
    
  } else {
    
    if(relative){
      # experimental & 
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = FALSE, 
                            relative = TRUE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = FALSE, 
                            relative = TRUE) %>%
        select(Protein, Sequence, Start, End, diff_frac_exch, err_frac_exch, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_frac_exch = round(diff_frac_exch, 4),
               err_frac_exch = round(err_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Frac Exch" = diff_frac_exch,
               "Err Diff Frac Exch" = err_frac_exch,
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
        select(Protein, Sequence, Start, End, abs_diff_frac_exch, err_abs_diff_frac_exch, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(abs_diff_frac_exch = round(abs_diff_frac_exch, 4),
               err_abs_diff_frac_exch = round(err_abs_diff_frac_exch, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Abs Value Exch" = abs_diff_frac_exch,
               "Err Diff Abs Value Exch" = err_abs_diff_frac_exch,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
    }
  }

  
}