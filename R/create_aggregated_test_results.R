#' Agregate test result
#' 
#' @description ...
#'
#' @importFrom data.table fifelse
#' 
#' @param p_diff_uptake_conf_dat ...
#' @param time_t ...
#' @param skip_amino \code{integer}, indicator
#' how many aminos from the N-termins should be 
#' omitted in visualization
#' 
#' @details Only peptides without modification are 
#' aggregated.
#' 
#' @return a \code{\link{data.frame}} object
#' 
#' @referneces ...
#'
#' @seealso 
#' \code{\link{read_hdx}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' p_diff_uptake_dat <- create_p_diff_uptake_dataset(dat)
#' p_diff_uptake_conf_dat <- create_p_diff_uptake_dataset_with_confidence(p_diff_uptake_dat)
#' create_aggregated_test_results(p_diff_uptake_conf_dat)
#' 
#' @export create_aggregated_test_results

create_aggregated_test_results <- function(p_diff_uptake_conf_dat, 
                                           time_t = 1,
                                           skip_amino = 1){
  
  p_diff_uptake_conf_dat <- as.data.table(p_diff_uptake_conf_dat)
  
  if(skip_amino > 0) { p_diff_uptake_conf_dat[, Start := Start + skip_amino] }
  
  p_diff_uptake_conf_dat <- p_diff_uptake_conf_dat[, sign := (diff_deut_uptake>=0) ]
  
  p_diff_uptake_conf_dat <- p_diff_uptake_conf_dat[Exposure == time_t, 
                                                   .( Start, End, ID, Exposure, valid, sign)]
  
  sequence_length <- max(unique(p_diff_uptake_conf_dat[["End"]]))
 
  res <- data.table(pos = 1:sequence_length,
                    count_valid = 0,
                    count_invalid = 0,
                    sign = 0)
  
  apply(p_diff_uptake_conf_dat, 1, function(peptide){
    
    res <- res[ pos >= peptide[["Start"]] & pos <= peptide[["End"]], `:=`(count_valid = count_valid + peptide[["valid"]],
                                                                          count_invalid = count_invalid + !peptide[["valid"]],
                                                                          sign = peptide[["sign"]]) ]
    
    
  })
   
  res[, valid := (count_valid - count_invalid > 0)]
  res[valid == T, result := fifelse(sign >=0, 1, -1)]
  res[valid == F, result := 0]
  
  res <- res[, .(pos, result)]
  setnames(res, c("pos",  "result"), c("Residues", paste0(time_t*60, "s")))
  res
}

