#' Agregates test result
#' 
#' @description ...
#' 
#' @param p_diff_uptake_conf_dat ...
#' 
#' @details Only peptides without modification are 
#' aggregated.
#' 
#' @return a \code{\link{data.frame}} object
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
                                           time_t = 1){
  
  p_diff_uptake_conf_dat <- as.data.table(p_diff_uptake_conf_dat)
  
  p_diff_uptake_conf_dat <- p_diff_uptake_conf_dat[, sign := (diff_deut_uptake>=0) ]
  
  p_diff_uptake_conf_dat <- p_diff_uptake_conf_dat[Exposure == time_t & is.na(Modification), .( Start, End, ID, Exposure, valid, sign)]
 
  res <- data.table(pos = 1:max(p_diff_uptake_conf_dat[["End"]]),
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
}

