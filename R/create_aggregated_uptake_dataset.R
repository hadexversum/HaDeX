#' Calculates the aggregated uptake for peptide pool
#'
#' @param kin_dat ...
#'
#' @examples
#' kin_dat <- create_uptake_dataset(alpha_dat, states = "Alpha_KSCN")
#' create_aggregated_uptake_dataset(kin_dat)
#'
#' @export

create_aggregated_uptake_dataset <- function(kin_dat){

  kin_dat <- as.data.table(kin_dat)
  times <- unique(kin_dat[["Exposure"]])

  uc_dataset <- lapply(times, function(time){

    uc_t <- calculate_aggregated_uptake(kin_dat,
                                        time_t = time)

  }) %>% bind_rows()

  return(uc_dataset)

}
