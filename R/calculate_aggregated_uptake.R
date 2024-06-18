#' Calculates aggregated deuterium uptake
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' kin_dat <- create_kin
#' 
#' @export

calculate_aggregated_uptake <- function(kin_dat, 
                                        time_t){
  
  kin_dat <- as.data.table(kin_dat)
  kin_dat_t <- kin_dat[Exposure == time_t]
  
  residues <- get_residue_positions(kin_dat)
  residues["frac_uc"] <- NA
  
  lapply(1:nrow(residues), function(i){
    
    x <- kin_dat_t[Start >= i & i <= End & State == state]
    x[, weight := 1/MaxUptake/sum(1/MaxUptake)]
    frac_uc = weighted.mean(x[["frac_deut_uptake"]], w = x[["weight"]])
    residues[i, "frac_uc"] <<- frac_uc
    
  }) 
  
  return(as.data.frame(residues))
  
}
