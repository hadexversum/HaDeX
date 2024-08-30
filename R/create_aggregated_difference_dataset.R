#' Calculates aggregated uptake difference for peptide pool
#'
#' @param diff_uptake_dat ...
#'
#' @examples
#' diff_uptake_dat <- create_diff_uptake_dataset(alpha_dat)
#' create_aggregated_diff_uptake_dataset(diff_uptake_dat)
#'
#' @export

create_aggregated_diff_uptake_dataset <- function(diff_uptake_dat){

  diff_uptake_dat <- as.data.table(diff_uptake_dat)
  times <- unique(diff_uptake_dat[["Exposure"]])

  diff_uptake_dataset <- lapply(times, function(time){

    diff_uc_t <- calculate_aggregated_diff_uptake(diff_uptake_dat,
                                                  time_t = time)
  }) %>% bind_rows()

  return(diff_uptake_dataset)
}
