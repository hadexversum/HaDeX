#' @examples 
#' 
#' 
#' @export

calculate_aggregated_diff_uptake <- function(diff_uptake_dat,
                                             time_t){
  
  diff_uptake_dat <- as.data.table(diff_uptake_dat)
  diff_uptake_dat <- diff_uptake_dat[Exposure == time_t]
  
  residues <- get_residue_positions(diff_uptake_dat)
  residues["diff_frac_uc"] <- NA
  residues["err_diff_frac_uc"] <- NA
  
  lapply(residues[["position"]], function(i){
    
    x <- diff_uptake_dat[Start <= i & i <= End]
    x[, weight := 1/MaxUptake/sum(1/MaxUptake)]
    x[, err_component := (weight * err_diff_frac_deut_uptake)^2]
    diff_frac_uc = weighted.mean(x[["diff_frac_deut_uptake"]], w = x[["weight"]], na.rm = T)
    err_diff_frac_uc = sqrt(sum(x[["err_component"]], na.rm = T))
    
    residues[i, "diff_frac_uc"] <<- diff_frac_uc
    residues[i, "err_diff_frac_uc"] <<- err_diff_frac_uc
    
  })
  
  residues["Exposure"] <- time_t
  
  return(as.data.frame(residues))
  
}