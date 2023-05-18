#' Differential deuterium uptake data
#' 
#' @description Present differential deuterium uptake values 
#' in selected form
#' 
#' @param diff_uptake_dat data produced by 
#' \code{\link{create_diff_uptake_dataset}} function
#' @param theoretical \code{logical}, indicator if values are 
#' calculated using theoretical controls
#' @param fractional \code{logical}, indicator if values are shown 
#' in fractional form 
#' @param renamed \code{logical}, indicator if the names of the columns
#' are renamed to user-friendly ones. Currently FALSE not implemented.
#' 
#' @details The function \code{\link{show_uptake_data}} generates a subsets
#' of the diff_uptake_dat based on selected parameters.
#' The numerical values are rounded to 4 places. The names of columns
#' are changed to user-friendly ones. 
#' 
#' @return a \code{\link{data.frame}} object
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
  
  diff_uptake_dat <- as.data.table(diff_uptake_dat)
  
  if(theoretical){
    
    if(fractional){
      
      diff_uptake_dat <- diff_uptake_dat[, .(Protein, ID, Sequence, Start, End, Modification, Exposure, 
                                             diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake)]
      diff_uptake_dat[, `:=`(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
                             err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4))]
      setorderv(diff_uptake_dat, cols = c("Start", "End"))
      diff_uptake_dat <- diff_uptake_dat[, c("Protein", "ID", "Sequence", "Modification", 
                                             "Start", "End", "Exposure", 
                                             "diff_theo_frac_deut_uptake", 
                                             "err_diff_theo_frac_deut_uptake")]
      setnames(diff_uptake_dat,
               c("diff_theo_frac_deut_uptake", "err_diff_theo_frac_deut_uptake"),
               c("Theo Frac Diff DU [%]", "U(Theo Frac Diff DU) [%]"))
      
    } else {
      
      diff_uptake_dat <- diff_uptake_dat[, .(Protein, ID, Sequence, Start, End, Modification, Exposure, 
                                             diff_theo_deut_uptake, err_diff_theo_deut_uptake)]
      diff_uptake_dat[, `:=`(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
                             err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4))]
      setorderv(diff_uptake_dat, cols = c("Start", "End"))
      diff_uptake_dat <- diff_uptake_dat[, c("Protein", "ID", "Sequence", "Modification", 
                                             "Start", "End", "Exposure", "diff_theo_deut_uptake", 
                                             "err_diff_theo_deut_uptake")]
      setnames(diff_uptake_dat,
               c("diff_theo_deut_uptake", "err_diff_theo_deut_uptake"),
               c("Theo Diff DU [Da]", "U(Theo Diff DU) [Da]"))
      
    }
    
  } else {
    
    if(fractional){
      
      diff_uptake_dat <- diff_uptake_dat[, .(Protein, ID, Sequence,  Start, End, Modification, Exposure, 
                                             diff_frac_deut_uptake, err_diff_frac_deut_uptake)]
      diff_uptake_dat[, `:=`(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
                             err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4))]
      setorderv(diff_uptake_dat, cols = c("Start", "End"))
      diff_uptake_dat <- diff_uptake_dat[, c("Protein", "ID", "Sequence", "Modification", 
                                             "Start", "End", "Exposure", "diff_frac_deut_uptake", 
                                             "err_diff_frac_deut_uptake")]
      setnames(diff_uptake_dat,
               c("diff_frac_deut_uptake", "err_diff_frac_deut_uptake"),
               c("Frac Diff DU [%]", "U(Frac Diff DU) [%]"))
      
    } else {
      
      diff_uptake_dat <- diff_uptake_dat[, .(Protein, ID, Sequence, Start, End, Modification, Exposure, 
                                             diff_deut_uptake, err_diff_deut_uptake)]
      diff_uptake_dat[, `:=`(diff_deut_uptake = round(diff_deut_uptake, 4),
                             err_diff_deut_uptake = round(err_diff_deut_uptake, 4))]
      setorderv(diff_uptake_dat, cols = c("Start", "End"))
      diff_uptake_dat <- diff_uptake_dat[, c("Protein", "ID", "Sequence", "Modification", 
                                             "Start", "End", "Exposure", "diff_deut_uptake", 
                                             "err_diff_deut_uptake")]
      setnames(diff_uptake_dat,
               c("diff_deut_uptake", "err_diff_deut_uptake"),
               c("Diff DU [Da]", "U(Diff DU) [Da]"))
      
    }
    
  }
  
  return(diff_uptake_dat)
  
}


#' Differential deuterium uptake data
#' 
#' @description Present differential deuterium uptake values 
#' in selected form
#' 
#' @param diff_uptake_dat data produced by 
#' \code{\link{create_diff_uptake_dataset}} function
#' @param theoretical \code{logical}, indicator if values are 
#' calculated using theoretical controls
#' @param fractional \code{logical}, indicator if values are shown 
#' in fractional form 
#' @param renamed \code{logical}, indicator if the names of the columns
#' are renamed to user-friendly ones. Currently FALSE not implemented.
#' 
#' @details The function \code{\link{show_uptake_data}} generates a subsets
#' of the diff_uptake_dat based on selected parameters.
#' The numerical values are rounded to 4 places. The names of columns
#' are changed to user-friendly ones. 
#' 
#' @return a \code{\link{data.frame}} object
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{create_diff_uptake_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' p_diff_uptake_dat <- create_p_diff_uptake_dataset(dat)
#' head(show_p_diff_uptake_data(p_diff_uptake_dat))
#' 
#' @export show_p_diff_uptake_data

show_p_diff_uptake_data <- function(p_diff_uptake_dat, 
                                    theoretical = FALSE, 
                                    fractional = FALSE,
                                    renamed = TRUE){
  
  p_diff_uptake_dat <- as.data.table(p_diff_uptake_dat)
  
  if(theoretical){
    
    if(fractional){
      
      p_diff_uptake_dat <- p_diff_uptake_dat[, .(Protein, ID, Sequence, Start, End, Modification, Exposure, 
                                                 diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake, P_value, log_p_value)]
      p_diff_uptake_dat[, `:=`(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
                               err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4),
                               P_value = round(P_value, 4),
                               log_p_value = round(log_p_value, 4))]
      setorderv(p_diff_uptake_dat, cols = c("Start", "End"))
      p_diff_uptake_dat <- p_diff_uptake_dat[, c("Protein", "ID", "Sequence", "Modification", 
                                             "Start", "End", "Exposure", 
                                             "diff_theo_frac_deut_uptake", 
                                             "err_diff_theo_frac_deut_uptake", "P_value", "log_p_value")]
      setnames(p_diff_uptake_dat,
               c("diff_theo_frac_deut_uptake", "err_diff_theo_frac_deut_uptake", "P_value", "log_p_value"),
               c("Theo Frac Diff DU [%]", "U(Theo Frac Diff DU) [%]", "P value", "log(P value)"))
      
    } else {
      
      p_diff_uptake_dat <- p_diff_uptake_dat[, .(Protein, ID, Sequence, Start, End, Modification, Exposure, 
                                                 diff_theo_deut_uptake, err_diff_theo_deut_uptake, P_value, log_p_value)]
      p_diff_uptake_dat[, `:=`(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
                               err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4),
                               P_value = round(P_value, 4),
                               log_p_value = round(log_p_value, 4))]
      setorderv(p_diff_uptake_dat, cols = c("Start", "End"))
      p_diff_uptake_dat <- p_diff_uptake_dat[, c("Protein", "ID", "Sequence", "Modification", 
                                             "Start", "End", "Exposure", "diff_theo_deut_uptake", 
                                             "err_diff_theo_deut_uptake", "P_value", "log_p_value")]
      setnames(p_diff_uptake_dat,
               c("diff_theo_deut_uptake", "err_diff_theo_deut_uptake", "P_value", "log_p_value"),
               c("Theo Diff DU [Da]", "U(Theo Diff DU) [Da]", "P value", "log(P value)"))
      
    }
    
  } else {
    
    if(fractional){
      
      p_diff_uptake_dat <- p_diff_uptake_dat[, .(Protein, ID, Sequence,  Start, End, Modification, Exposure, 
                                                 diff_frac_deut_uptake, err_diff_frac_deut_uptake, P_value, log_p_value)]
      p_diff_uptake_dat[, `:=`(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
                               err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4),
                               P_value = round(P_value, 4),
                               log_p_value = round(log_p_value, 4))]
      setorderv(p_diff_uptake_dat, cols = c("Start", "End"))
      p_diff_uptake_dat <- p_diff_uptake_dat[, c("Protein", "ID", "Sequence", "Modification", 
                                             "Start", "End", "Exposure", "diff_frac_deut_uptake", 
                                             "err_diff_frac_deut_uptake", "P_value", "log_p_value")]
      setnames(p_diff_uptake_dat,
               c("diff_frac_deut_uptake", "err_diff_frac_deut_uptake", "P_value", "log_p_value"),
               c("Frac Diff DU [%]", "U(Frac Diff DU) [%]", "P value", "log(P value)"))
      
    } else {
      
      p_diff_uptake_dat <- p_diff_uptake_dat[, .(Protein, ID, Sequence, Start, End, Modification, Exposure, 
                                                 diff_deut_uptake, err_diff_deut_uptake, P_value, log_p_value)]
      p_diff_uptake_dat[, `:=`(diff_deut_uptake = round(diff_deut_uptake, 4),
                               err_diff_deut_uptake = round(err_diff_deut_uptake, 4),
                               P_value = round(P_value, 4),
                               log_p_value = round(log_p_value, 4))]
      setorderv(p_diff_uptake_dat, cols = c("Start", "End"))
      p_diff_uptake_dat <- p_diff_uptake_dat[, c("Protein", "ID", "Sequence", "Modification", 
                                             "Start", "End", "Exposure", "diff_deut_uptake", 
                                             "err_diff_deut_uptake", "P_value", "log_p_value")]
      setnames(p_diff_uptake_dat,
               c("diff_deut_uptake", "err_diff_deut_uptake", "P_value", "log_p_value"),
               c("Diff DU [Da]", "U(Diff DU) [Da]", "P value", "log(P value)"))
      
    }
    
  }
  
  return(p_diff_uptake_dat)
  
}