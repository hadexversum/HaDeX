#' Deuterium uptake data
#' 
#' @description Present deuterium uptake values 
#' in selected form
#' 
#' @importFrom dplyr rename %>% cur_group_id
#' 
#' @param uptake_dat data produced by \code{\link{create_uptake_dataset}} 
#' function or \code{\link{create_state_uptake_dataset}}
#' @param theoretical \code{logical}, indicator if values are 
#' calculated using theoretical controls
#' @param fractional \code{logical}, indicator if values are shown 
#' in fractional form 
#' @param renamed \code{logical}, indicator if the names of the columns
#' are renamed to user-friendly ones. Currently FALSE not implemented.
#' 
#' @details The function \code{\link{show_uptake_data}} generates a subsets
#' of the uptake_dat based on selected parameters.
#' The numerical values are rounded to 4 places. The names of columns
#' are changed to user-friendly ones. 
#' 
#' @return a \code{\link{data.frame}} object
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
  
  uptake_dat <- as.data.table(uptake_dat)
  
  if (theoretical){
    
    if (fractional){
      
      uptake_dat <- uptake_dat[, .(Protein, Sequence, State, Start, End, Exposure, Modification,
                                   theo_frac_deut_uptake, err_theo_frac_deut_uptake)]
      uptake_dat[, `:=`(theo_frac_deut_uptake  = round(theo_frac_deut_uptake , 4),
                        err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4))]
      setorderv(uptake_dat, cols = c("Start", "End"))
      uptake_dat[, ID := .GRP, by = c("Start", "End", "Sequence")]
      setnames(uptake_dat,
               c("theo_frac_deut_uptake", "err_theo_frac_deut_uptake"),
               c("Theo Frac DU [%]", "U(Theo Frac DU) [%]"))
      uptake_dat <- uptake_dat[, c("Protein", "Sequence", "ID", "Modification", "State", "Start", 
                                   "End", "Exposure", "Theo Frac DU [%]",
                                   "U(Theo Frac DU) [%]")]
      
    } else {
      
      uptake_dat <- uptake_dat[, .(Protein, Sequence, State, Start, End, Exposure, Modification,
                                   theo_deut_uptake, err_theo_deut_uptake)]
      uptake_dat[, `:=`(theo_deut_uptake = round(theo_deut_uptake, 4),
                        err_theo_deut_uptake = round(err_theo_deut_uptake, 4))]
      setorderv(uptake_dat, cols = c("Start", "End"))
      uptake_dat[, ID := .GRP, by = c("Start", "End", "Sequence")]
      uptake_dat <- uptake_dat[, c("Protein", "Sequence", "ID", "Modification", "State", "Start", 
                                   "End", "Exposure", "theo_deut_uptake",
                                   "err_theo_deut_uptake")]
      setnames(uptake_dat,
               c("theo_deut_uptake", "err_theo_deut_uptake"),
               c("Theo DU [Da]", "U(Theo DU) [Da]"))
    }
    
  } else {
    
    if (fractional){
      
      uptake_dat <- uptake_dat[, .(Protein, Sequence, State, Start, End, Exposure, Modification,
                                   frac_deut_uptake, err_frac_deut_uptake)]
      uptake_dat[, `:=`(frac_deut_uptake = round(frac_deut_uptake, 4),
                        err_frac_deut_uptake = round(err_frac_deut_uptake, 4))]
      setorderv(uptake_dat, cols = c("Start", "End"))
      uptake_dat[, ID := .GRP, by = c("Start", "End", "Sequence")]
      uptake_dat <- uptake_dat[, c("Protein", "Sequence", "ID", "Modification", "State", "Start", 
                                   "End", "Exposure", "frac_deut_uptake",
                                   "err_frac_deut_uptake")]
      setnames(uptake_dat,
               c("frac_deut_uptake", "err_frac_deut_uptake"),
               c("Frac DU [%]", "U(Frac DU) [%]"))
      
    } else {
      
      uptake_dat <- uptake_dat[, .(Protein, Sequence, State, Start, End, Exposure, Modification,
                                   deut_uptake, err_deut_uptake)]
      uptake_dat[, `:=`(deut_uptake = round(deut_uptake, 4),
                        err_deut_uptake = round(err_deut_uptake, 4))]
      setorderv(uptake_dat, cols = c("Start", "End"))
      uptake_dat[, ID := .GRP, by = c("Start", "End", "Sequence")]
      uptake_dat <- uptake_dat[, c("Protein", "Sequence", "ID", "Modification", "State", "Start", 
                                   "End", "Exposure", "deut_uptake", "err_deut_uptake")]
      setnames(uptake_dat,
               c("deut_uptake", "err_deut_uptake"),
               c("DU [Da]", "U(DU) [Da]"))
      
    }
  }
  
  return(uptake_dat)
  
}





#' Differential uptake data with confidence 
#' 
#' @description Present differential deuterium uptake values 
#' in selected form, accompanied by the significance
#' 
#' @param diff_uptake_dat data produced by \code{\link{create_diff_uptake_dataset}}
#' function
#' @param theoretical \code{logical}, indicator if values are 
#' calculated using theoretical controls
#' @param fractional \code{logical}, indicator if values are shown 
#' in fractional form 
#' @param confidence_level confidence level for the test, from range [0, 1].
#' @param hybrid \code{logical}, indicator if the hybrid testing was
#' applied in diff_uptake_dat. 
#' 
#' @details The function \code{\link{show_uptake_data}} generates a subsets
#' of the uptake dat based on selected parameters. It contains the information
#' if the value is statistically significant at selected confidence level.
#' The numerical values are rounded to 4 places. The names of columns
#' are changed to user-friendly ones. 
#' 
#' @return a \code{\link{data.frame}} object
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
                                             confidence_level = 0.98,
                                             hybrid = FALSE){
  
  column_name_cl1 <- paste0("Valid At ", confidence_level)
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional  
      diff_uptake_dat <- data.table(add_stat_dependency(diff_uptake_dat,
                                                        confidence_level = confidence_level,
                                                        theoretical = TRUE, 
                                                        fractional = TRUE))
      diff_uptake_dat <- diff_uptake_dat[, mget(c("Protein", "Sequence", "ID", "Modification", 
                                                  'Start', "End", "Exposure", 
                                                  "diff_theo_frac_deut_uptake", 
                                                  "err_diff_theo_frac_deut_uptake", 
                                                  paste0("valid_at_", confidence_level)))]
      diff_uptake_dat[, `:=`(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
                             err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4))]
      setorderv(diff_uptake_dat, cols = c("Start", "End"))
      setnames(diff_uptake_dat, 
               c("diff_theo_frac_deut_uptake",  "err_diff_theo_frac_deut_uptake", paste0("valid_at_", confidence_level)),
               c("Theo Frac Diff DU [%]", "U(Theo Frac Diff DU) [%]", column_name_cl1), 
               skip_absent = TRUE)
      
    } else {
      # theoretical & absolute
      diff_uptake_dat <- data.table(add_stat_dependency(diff_uptake_dat,
                                                        confidence_level = confidence_level,
                                                        theoretical = TRUE, 
                                                        fractional = TRUE))
      diff_uptake_dat <- diff_uptake_dat[, mget(c("Protein", "Sequence", "ID", "Modification", 
                                                  'Start', "End", "Exposure", 
                                                  "diff_theo_deut_uptake", 
                                                  "err_diff_theo_deut_uptake", 
                                                  paste0("valid_at_", confidence_level)))]
      diff_uptake_dat[, `:=`(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
                             err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4))]
      setorderv(diff_uptake_dat, cols = c("Start", "End"))
      setnames(diff_uptake_dat, 
               c("diff_theo_deut_uptake",  "err_diff_theo_deut_uptake", paste0("valid_at_", confidence_level)),
               c("Theo Diff DU [Da]", "U(Theo Diff DU) [Da]", column_name_cl1), 
               skip_absent = TRUE)
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      diff_uptake_dat <- data.table(add_stat_dependency(diff_uptake_dat,
                                                        confidence_level = confidence_level,
                                                        theoretical = TRUE, 
                                                        fractional = TRUE))
      diff_uptake_dat <- diff_uptake_dat[, mget(c("Protein", "Sequence", "ID", "Modification", 
                                                  'Start', "End", "Exposure", 
                                                  "diff_frac_deut_uptake", 
                                                  "err_diff_frac_deut_uptake", 
                                                  paste0("valid_at_", confidence_level)))]
      diff_uptake_dat[, `:=`(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
                             err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4))]
      setorderv(diff_uptake_dat, cols = c("Start", "End"))
      setnames(diff_uptake_dat, 
               c("diff_frac_deut_uptake",  "err_diff_frac_deut_uptake", paste0("valid_at_", confidence_level)),
               c("Frac Diff DU [%]", "U(Frac Diff DU) [%]", column_name_cl1), 
               skip_absent = TRUE)
      
    } else {
      # experimental & absolute
      diff_uptake_dat <- data.table(add_stat_dependency(diff_uptake_dat,
                                                        confidence_level = confidence_level,
                                                        theoretical = TRUE, 
                                                        fractional = TRUE))
      diff_uptake_dat <- diff_uptake_dat[, mget(c("Protein", "Sequence", "ID", "Modification", 
                                                  'Start', "End", "Exposure", 
                                                  "diff_deut_uptake", 
                                                  "err_diff_deut_uptake", 
                                                  paste0("valid_at_", confidence_level)))]
      diff_uptake_dat[, `:=`(diff_deut_uptake = round(diff_deut_uptake, 4),
                             err_diff_deut_uptake = round(err_diff_deut_uptake, 4))]
      setorderv(diff_uptake_dat, cols = c("Start", "End"))
      setnames(diff_uptake_dat, 
               c("diff_deut_uptake",  "err_diff_deut_uptake", paste0("valid_at_", confidence_level)),
               c("Diff DU [Da]", "U(Diff DU) [Da]", column_name_cl1), 
               skip_absent = TRUE)
    }
  }
  
  return(diff_uptake_dat)
  
}



#' Summary data
#' 
#' @description Show summary table
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function
#' @param confidence_level confidence level for the test, from range [0, 1].
#' @param protein_length the length of the protein sequence
#' 
#' @details The format in the table is suggested by the community and 
#' should be provided for experimetal data.
#' 
#' @return a \code{\link{data.table}} object
#' 
#' @references Masson, G.R., Burke, J.E., Ahn, N.G., Anand, G.S., Borchers, C., 
#' Brier, S., Bou-Assaf, G.M., Engen, J.R., Englander, S.W., Faber, J., 
#' et al. (2019). Recommendations for performing, interpreting and reporting 
#' hydrogen deuterium exchange mass spectrometry (HDX-MS) experiments. 
#' Nat Methods 16, 595–602
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' show_summary_data(dat)
#' 
#' @export show_summary_data

show_summary_data <- function(dat, 
                              confidence_level = "",
                              protein_length = max(dat[["End"]])){
  
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
                       paste0(get_protein_coverage(dat, protein_length = protein_length), "%"),
                       round(mean(nchar(unique(dat[["Sequence"]]))), 2),# average peptide length
                       get_protein_redundancy(dat, protein_length),
                       get_n_replicates(dat), 
                       #NA, 
                       confidence_level)
  )
  
}