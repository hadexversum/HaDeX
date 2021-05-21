

#' Generate chiclet differential data
#' 
#' @param chiclet_diff_dat produced by 
#' \code{\link{generate_butterfly_differential_dataset}} function. 
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' 
#' @details This function subsets the dataset based on provided criteria,
#' rounds the numerical values (4 places) and changes the column names 
#' to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso
#' \code{\link{generate_butterfly_differential_dataset}}
#' \code{\link{generate_chiclet_differential_plot}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' chic_diff_dat <- generate_butterfly_differential_dataset(dat)
#' head(generate_chiclet_differential_data(chic_diff_dat))
#' 
#' @export generate_chiclet_differential_data

generate_chiclet_differential_data <- function(chiclet_diff_dat, 
                                               theoretical = FALSE, 
                                               fractional = FALSE){
  
  if(theoretical){
    
    if(fractional){
      
      chiclet_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake) %>%
        mutate(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
               err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Theo Frac DU" = diff_theo_frac_deut_uptake,
               "Err Diff Theo Frac DU" = err_diff_theo_frac_deut_uptake)
      
    } else {
      
      chiclet_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_deut_uptake, err_diff_theo_deut_uptake) %>%
        mutate(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
               err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Theo DU" = diff_theo_deut_uptake,
               "Err Diff Theo DU" = err_diff_theo_deut_uptake)
      
    }
    
  } else {
    
    if(fractional){
      
      chiclet_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_frac_deut_uptake, err_diff_frac_deut_uptake) %>%
        mutate(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
               err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Frac DU" = diff_frac_deut_uptake,
               "Err Diff Frac DU" = err_diff_frac_deut_uptake)
      
    } else {
      
      chiclet_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_deut_uptake, err_diff_deut_uptake) %>%
        mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
               err_diff_deut_uptake = round(err_diff_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff DU" = diff_deut_uptake,
               "Err Diff DU" = err_diff_deut_uptake)
      
    }
    
  }
  
}