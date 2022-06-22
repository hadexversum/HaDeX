#' Create dataset with control
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param control_protein maximal exchange control protein, from dat. 
#' @param control_state maximal exchange control state, from dat.
#' @param control_exposure maximal exchange control exposure (time
#' point of measurement), from dat.
#' 
#' @details Function \code{\link{create_control_dataset}}
#' creates a dataset (similar to the output of \code{\link{read_hdx}} 
#' function), with maximal exchange control for all the states,
#' based on provided parameters. The other functions are operating 
#' within a state, so the control is prepared for each state. 
#' The chosen maximal exchange control is distinguishable by the value 
#' `99999` in `Exposure` control. 
#' 
#' @return a \code{\link{data.frame}} object. 
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' create_control_dataset(dat)
#' 
#' @export create_control_dataset

create_control_dataset <- function(dat,
                                   control_protein = dat[["Protein"]][1],
                                   control_state = dat[["State"]][1],
                                   control_exposure = max(dat[["Exposure"]])){
  
  tmp <- dat[Protein == control_protein & State == control_state & Exposure == control_exposure][, Exposure := 99999]
  states_to_prepare <- unique(dat[Protein == control_protein][["State"]])
  
  control_dat <- rbind(dat,
                       rbindlist(lapply(states_to_prepare, function(state){
                         
                         peps <- unlist(unique(dat[State == state, .(Sequence)]))
                         tmp[Sequence %in% peps][, State := state]
                         
                       }))
  )
  
  attr(control_dat, "control_protein") <- control_protein 
  attr(control_dat, "control_state") <- control_state 
  attr(control_dat, "control_exposure") <- control_exposure
  attr(control_dat, "n_rep") <- attr(dat, "n_rep")
  
  return(control_dat)
  
}