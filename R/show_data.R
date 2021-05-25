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
#' @importFrom dplyr rename %>%
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
        select(Protein, Sequence, ID, State, Start, End, Exposure, theo_frac_deut_uptake, err_theo_frac_deut_uptake) %>%
        mutate(theo_frac_deut_uptake  = round(theo_frac_deut_uptake , 4),
               err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Frac Exch" = theo_frac_deut_uptake , 
               "Err Theo Frac Exch" = err_theo_frac_deut_uptake)
      
    } else {
      
      uptake_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, theo_deut_uptake, err_theo_deut_uptake) %>%
        mutate(theo_deut_uptake = round(theo_deut_uptake, 4),
               err_theo_deut_uptake = round(err_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Abs Val Exch" = theo_deut_uptake,
               "Err Theo Abs Val Exch" = err_theo_deut_uptake)
    }
    
  } else {
    
    if (fractional){
      
      uptake_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, frac_deut_uptake, err_frac_deut_uptake) %>%
        mutate(frac_deut_uptake = round(frac_deut_uptake, 4),
               err_frac_deut_uptake = round(err_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Frac Exch" = frac_deut_uptake,
               "Err Frac Exch" = err_frac_deut_uptake)
      
    } else {
      
      uptake_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, deut_uptake, err_deut_uptake) %>%
        mutate(deut_uptake = round(deut_uptake, 4),
               err_deut_uptake = round(err_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Abs Val Exch" = deut_uptake,
               "Err Abs Val Exch" = err_deut_uptake)
      
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
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake) %>%
        mutate(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
               err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Theo Frac DU" = diff_theo_frac_deut_uptake,
               "Err Diff Theo Frac DU" = err_diff_theo_frac_deut_uptake)
      
    } else {
      
      diff_uptake_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_deut_uptake, err_diff_theo_deut_uptake) %>%
        mutate(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
               err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Theo DU" = diff_theo_deut_uptake,
               "Err Diff Theo DU" = err_diff_theo_deut_uptake)
      
    }
    
  } else {
    
    if(fractional){
      
      diff_uptake_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_frac_deut_uptake, err_diff_frac_deut_uptake) %>%
        mutate(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
               err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Frac DU" = diff_frac_deut_uptake,
               "Err Diff Frac DU" = err_diff_frac_deut_uptake)
      
    } else {
      
      diff_uptake_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_deut_uptake, err_diff_deut_uptake) %>%
        mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
               err_diff_deut_uptake = round(err_diff_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff DU" = diff_deut_uptake,
               "Err Diff DU" = err_diff_deut_uptake)
      
    }
    
  }
  
}

#' Show volcano data 
#'  
#' @param vol_data data produced by the \code{\link{create_volcano_dataset}} 
#' function.
#'
#' @details This function subsets the dataset based on provided criteria,
#' rounds the numerical values (4 places) and changes the column names 
#' to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @return a \code{\link{data.frame}} object.
#'
#' @seealso 
#' \code{\link{create_volcano_dataset}} 
#' \code{\link{plot_volcano}} 
#' 
#' @examples 
#' dat <-  dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' vol_dat <- create_volcano_dataset(dat)
#' head(show_volcano_data(vol_dat))
#' 
#' @export show_volcano_data

show_volcano_data <- function(vol_data){
  
  vol_data %>%
    mutate(D_diff  = round(D_diff , 4),
           Uncertainty = round(Uncertainty, 4),
           log_p_value = round(log_p_value, 4)) %>%
    arrange(Exposure, Start, End) %>%
    rename("Deuterium uptake difference" = D_diff , 
           "-log(P value)" = log_p_value)
  
}
