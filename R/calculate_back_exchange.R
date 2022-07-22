#' Back exchange estimation
#'
#' @description ...
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function
#' @param protein selected protein
#' @param states selected biological states for given protein
#' @param time_100 time point of measurement for fully 
#' deuterated sample
#'
#' @details ...
#'
#' @return a \code{\link{data.frame}} object
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' calculate_back_exchange(dat)
#' 
#' @export calculate_back_exchange

calculate_back_exchange <- function(dat, 
                                    protein = dat[["Protein"]][1],
                                    states = unique(dat[["State"]]),
                                    time_100 = max(dat[["Exposure"]])){
  
  dat <- as.data.table(dat)
  
  dat <- dat[Protein == protein & State %in% states]
  
  uptake_dat <- create_uptake_dataset(dat)
  
  uptake_dat <- as.data.table(uptake_dat)
  
  uptake_dat <- uptake_dat[Exposure == time_100, .(Protein, Start, End, State, Sequence, Modification, theo_frac_deut_uptake, err_theo_frac_deut_uptake)]
  
  bex_dat <-  uptake_dat[, `:=`(back_exchange = (100 - theo_frac_deut_uptake),
                                err_back_exchange = err_theo_frac_deut_uptake)]
  
  bex_dat <- bex_dat[, .(Protein, Start, End, State, Sequence, Modification, back_exchange, err_back_exchange)]

  as.data.frame(bex_dat)
  
}
