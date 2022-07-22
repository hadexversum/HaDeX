#' Generate differential dataset
#' 
#' @description Calculates differential deuterium uptake values 
#' between two states. 
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state_1 biological state for chosen protein. From this state values
#' the second state values are subtracted to get the deuterium uptake difference.
#' @param state_2 biological state for chosen protein. This state values are 
#' subtracted from the first state values to get the deuterium uptake difference.
#' @param time_0 minimal exchange control time point of measurement [min].
#' @param time_100 maximal exchange control time point of measurement [min]. 
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details The function \code{\link{create_diff_uptake_dataset}} 
#' generates a dataset with differential values between given biological states 
#' (state_1 - state_2). For each peptide of chosen protein for time points of 
#' measurement between minimal and maximal control time points of measurement 
#' deuterium uptake difference, fractional deuterium uptake difference with 
#' respect to controls or theoretical tabular values are calculated, with 
#' combined and propagated uncertainty. Each peptide has an ID, based on its start
#' position.
#' Function output can be visualized as a differential (Woods) plot, butterfly
#' differential plot or chiclet differential plot. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_diff_uptake}}
#' \code{\link{plot_differential_butterfly}}
#' \code{\link{plot_differential_chiclet}}
#' \code{\link{plot_differential}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' head(diff_uptake_dat)
#' 
#' @export create_diff_uptake_dataset

create_diff_uptake_dataset <- function(dat, 
                                       protein = unique(dat[["Protein"]])[1],
                                       state_1 = unique(dat[["State"]])[1],
                                       state_2 = unique(dat[["State"]])[2], 
                                       time_0 = min(dat[["Exposure"]]),
                                       time_100 = max(dat[["Exposure"]]),
                                       deut_part = 0.9){
  
  ## temporarily, for compatibility
  n_rep <- attr(dat, "n_rep")
  dat <- data.table(dat)
  ##
  
  all_times <- unique(dat[["Exposure"]])
  times <- all_times[all_times > time_0 & all_times <= time_100]
  
  
  diff_uptake_dat <- rbindlist(lapply(times, function(time){
    
    dt <- setorderv(data.table(calculate_diff_uptake(dat = dat,  ## temporarily, for compatibility
                                                     states = c(state_1, state_2), 
                                                     protein = protein, 
                                                     time_0 = time_0, time_t = time, time_100 = time_100,
                                                     deut_part = deut_part)), cols = c("Start", "End"))
    dt[, `:=`(ID = 1L:nrow(dt), Exposure = time)]
    
    col_order <- c("ID", "Exposure", setdiff(colnames(dt), c("ID", "Exposure")))
    setcolorder(dt, col_order)
    
  }))
  
  ## temporarily, for compatibility
  diff_uptake_dat <- data.frame(diff_uptake_dat) 
  ##
  
  attr(diff_uptake_dat, "protein") <- protein
  attr(diff_uptake_dat, "state_1") <- state_1
  attr(diff_uptake_dat, "state_2") <- state_2
  attr(diff_uptake_dat, "time_0") <- time_0
  attr(diff_uptake_dat, "time_100") <- time_100
  attr(diff_uptake_dat, "deut_part") <- deut_part
  attr(diff_uptake_dat, "has_modification") <- attr(dat, "has_modification")
  attr(diff_uptake_dat, "n_rep") <- n_rep
  
  return(diff_uptake_dat)
  
}