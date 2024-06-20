#' Calculates aggregated deuterium uptake
#'
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' kin_dat <- create_kin
#'
#' @export

calculate_aggregated_uptake <- function(kin_dat,
                                        state = unique(kin_dat[["State"]])[1],
                                        time_t){

  kin_dat <- as.data.table(kin_dat)
  kin_dat_t <- kin_dat[Exposure == time_t]

  residues <- get_residue_positions(kin_dat)
  residues["frac_uc"] <- NA
  residues["err_frac_uc"] <- NA

  lapply(residues[["position"]], function(i){

    x <- kin_dat_t[Start <= i & i <= End & State == state]
    x[, weight := 1/MaxUptake/sum(1/MaxUptake)]
    x[, err_component := (weight * err_frac_deut_uptake)^2]
    frac_uc = weighted.mean(x[["frac_deut_uptake"]], w = x[["weight"]])
    err_frac_uc = sqrt(sum(x[["err_component"]]))
    
    # residues[residues["position"] == i, "frac_uc"] <<- frac_uc
    # residues[residues["position"] == i, "err_frac_uc"] <<- err_frac_uc
    
    residues[i, "frac_uc"] <<- frac_uc
    residues[i, "err_frac_uc"] <<- err_frac_uc
    
  })

  residues["Exposure"] <- time_t

  return(as.data.frame(residues))

}
