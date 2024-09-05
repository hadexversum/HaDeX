#' Calculates aggregated deuterium uptake difference for one time point
#'
#' @param diff_uptake_dat ...
#' @param time_t ...
#'
#' @examples
#' diff_uptake_dat <- create_diff_uptake_dataset(alpha_dat)
#' calculate_aggregated_diff_uptake(diff_uptake_dat, time_t = 5)
#'
#' @export

calculate_aggregated_diff_uptake <- function(diff_uptake_dat,
                                             time_t){

  diff_uptake_dat <- as.data.table(diff_uptake_dat)
  diff_uptake_dat <- diff_uptake_dat[Exposure == time_t]

  residues <- get_residue_positions(diff_uptake_dat)
  residues["diff_frac_deut_uptake"] <- NA
  residues["err_diff_frac_deut_uptake"] <- NA

  residues["diff_deut_uptake"] <- NA
  residues["diff_theo_frac_deut_uptake"] <- NA
  residues["diff_theo_deut_uptake"] <- NA

  # residues

  lapply(residues[["position"]], function(i){

    x <- diff_uptake_dat[Start <= i & i <= End]
    x[, weight := 1/MaxUptake/sum(1/MaxUptake)]
    x[, err_component := (weight * err_diff_frac_deut_uptake)^2]
    diff_frac_uc = weighted.mean(x[["diff_frac_deut_uptake"]], w = x[["weight"]], na.rm = T)
    err_diff_frac_uc = sqrt(sum(x[["err_component"]], na.rm = T))

    residues[i, "diff_frac_deut_uptake"] <<- diff_frac_uc
    residues[i, "err_diff_frac_deut_uptake"] <<- err_diff_frac_uc

    residues[i, "diff_deut_uptake"] <<- weighted.mean(x[["diff_deut_uptake"]], w = x[["weight"]], na.rm = T)
    residues[i, "diff_theo_frac_deut_uptake"] <<- weighted.mean(x[["diff_theo_frac_deut_uptake"]], w = x[["weight"]], na.rm = T)
    residues[i, "diff_theo_deut_uptake"] <<- weighted.mean(x[["diff_theo_deut_uptake"]], w = x[["weight"]], na.rm = T)

  })

  residues["Exposure"] <- time_t

  return(as.data.frame(residues))

}
