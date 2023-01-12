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
#' @references ...
#'
#' @seealso 
#' \code{\link{read_hdx}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' p_diff_uptake_dat <- create_p_diff_uptake_dataset(dat)
#' p_diff_uptake_conf_dat <- create_p_diff_uptake_dataset_with_confidence(p_diff_uptake_dat)
#' calculate_aggregated_test_results(p_diff_uptake_conf_dat, method = "significance")
#' calculate_aggregated_test_results(p_diff_uptake_conf_dat, method = "weiss")
#' 
#' @export calculate_aggregated_test_results

calculate_aggregated_test_results <- function(p_diff_uptake_conf_dat, 
                                              method = c("significance", "weiss"),
                                              time_t = 1,
                                              skip_amino = 1){
  
  p_diff_uptake_conf_dat <- as.data.table(p_diff_uptake_conf_dat)
  
  if(skip_amino > 0) { p_diff_uptake_conf_dat[, Start := Start + skip_amino] }
  
  p_diff_uptake_conf_dat[, sign := (diff_deut_uptake>=0) ]
  
  sequence_length <- max(unique(p_diff_uptake_conf_dat[["End"]]))
  
  res <- NULL
  
  if(method == "significance") {
    
    tmp_dat <- p_diff_uptake_conf_dat[Exposure == time_t, .( Start, End, ID, Exposure, valid, sign)]
    
    res <- data.table(pos = 1:sequence_length,
                      count_valid = 0,
                      count_invalid = 0,
                      sign = 0)
    
    apply(tmp_dat, 1, function(peptide){
      
      res <- res[ pos >= peptide[["Start"]] & pos <= peptide[["End"]], `:=`(count_valid = count_valid + peptide[["valid"]],
                                                                            count_invalid = count_invalid + !peptide[["valid"]],
                                                                            sign = peptide[["sign"]]) ]
    })
    
    res[, valid := (count_valid - count_invalid > 0)]
    res[valid == T, result := fifelse(sign >=0, 1, -1)]
    res[valid == F, result := 0]
    
    res <- res[, .(pos, result)]
    setnames(res, c("pos",  "result"), c("Residues", paste0(time_t*60, "s")))
    
    
  } else if (method == "weiss"){
    
    tmp_dat <- p_diff_uptake_conf_dat[Exposure == time_t, .(Protein, Sequence, ID, Exposure, Start, End, diff_deut_uptake )]
    tmp_dat[, peptide_length := End - Start + 1]
    
    res <- create_hr_template(tmp_dat)
    res[["value"]] <- NaN
    
    apply(res, 1, function(amino){
      
      amino_pos <- as.numeric(amino[["pos"]])
      
      if(amino[["aa"]] %in% c("x", "P")) {
        
        res[pos == amino_pos, ][["value"]] <<- 0
        
      } else {
        
        tmp_peptide_dat <- tmp_dat[amino_pos >= Start & amino_pos <= End, ]
        tmp_peptide_dat[["n_exchangeable"]] <- calculate_n_exchangeable(tmp_peptide_dat[["Sequence"]])
        tmp_peptide_dat[["weight"]] <- calculate_weiss_weight(tmp_peptide_dat[["n_exchangeable"]])
        
        res[pos == amino_pos, ][["value"]] <<- weighted.mean(tmp_peptide_dat[["diff_deut_uptake"]], tmp_peptide_dat[["weight"]], na.rm = T)
      }
    })
  }
  
  res <- as.data.frame(res)
  
  return(res)
  
}

#' internal
calculate_n_exchangeable <- function(sequence,
                                     skip_amino = 0){
  nchar(sequence) - 1 - skip_amino - stri_count(substr(sequence, 2+skip_amino, nchar(sequence)), fixed = "P")
}

#' internal 
calculate_weiss_weight <- function(x){
  x^(-2)
}

#' internal
create_hr_template <- function(x_dat,
                               protein = x_dat[["Protein"]][1]){
  
  protein_sequence <- reconstruct_sequence(x_dat)
  
  return(
    data.table(
      pos = 1:nchar(protein_sequence),
      aa = unlist(tstrsplit(protein_sequence, ""))
    )
  )
}
