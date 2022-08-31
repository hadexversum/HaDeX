#' Create uptake dataset for chosen state
#' 
#' @description Calculates deuterium uptake values for one 
#' biological state.
#' 
#' @importFrom data.table setcolorder
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state biological state for chosen protein.
#' @param time_0 minimal exchange control time point of measurement [min].
#' @param time_100 maximal exchange control time point of measurement [min]. 
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details The function \code{\link{create_uptake_dataset}} generates 
#' a dataset with deuterium uptake values in different forms. For each
#' peptide in chosen protein in chosen state for time points of measurement
#' between minimal and maximal control time points of measurement deuterium 
#' uptake, fractional deuterium uptake with respect to controls or theoretical
#' tabular values are calculated, with combined and propagated uncertainty. 
#' Each peptide has an ID, based on its start position. 
#' This data can be presented in a form of comparison plot, butterfly plot or
#' chiclet plot. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' state_uptake_dat <- create_state_uptake_dataset(dat)
#' head(state_uptake_dat)
#' 
#' @export create_state_uptake_dataset

create_state_uptake_dataset <- function(dat, 
                                        protein = unique(dat[["Protein"]])[1],
                                        state = (dat[["State"]])[1], 
                                        time_0 = min(dat[dat[["Exposure"]]>0, ][["Exposure"]]),
                                        time_100 = max(dat[["Exposure"]]),
                                        deut_part = 0.9){
  
  all_times <- unique(dat[["Exposure"]])
  times <- all_times[all_times > time_0 & all_times <= time_100]
  
  state_uptake_dat <- rbindlist(lapply(times, function(time){
    
    uptake_dat <- setorderv(calculate_state_uptake(dat, protein = protein, state = state,
                                                   time_0 = time_0, time_t = time, time_100 = time_100,
                                                   deut_part = deut_part), cols = c("Start", "End"))
    uptake_dat[["ID"]] <- 1:nrow(uptake_dat)
    uptake_dat[["Exposure"]] <- time
    col_order <- c("ID", "Exposure", setdiff(colnames(uptake_dat), c("ID", "Exposure")))
    setcolorder(uptake_dat, col_order)
    
  }))
  
  attr(state_uptake_dat, "protein") <- protein
  attr(state_uptake_dat, "state") <- state
  attr(state_uptake_dat, "time_0") <- time_0
  attr(state_uptake_dat, "time_100") <- time_100
  attr(state_uptake_dat, "deut_part") <- deut_part
  
  return(state_uptake_dat)
  
}