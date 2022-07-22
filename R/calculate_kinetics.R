#' Calculate kinetics data 
#' 
#' @description Calculate kinetics of the hydrogen-deuteration exchange 
#' for given peptide in given state.
#' 
#' @importFrom dplyr %>% bind_rows mutate everything
#' @importFrom checkmate assert_data_frame assert_string assert_number
#' 
#' @param dat dat data imported by the \code{\link{read_hdx}} function.
#' @param protein protein chosen protein. 
#' @param sequence sequence of chosen peptide.
#' @param state biological state of chosen peptide.
#' @param start start position of chosen peptide.
#' @param end end position of chosen peptide.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement.
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#'
#' @details The function calculates deuteration data for all available data points 
#' for given peptide in chosen biological state.. 
#' All four variants (relative & theoretical combinations) of deuterium uptake computations 
#' are supported. Manual correction of percentage of deuterium the protein was exposed 
#' to during the exchange in theoretical calculations is provided.
#' To visualize obtained data we recommend using \code{\link{plot_uptake_curve}} function.
#' The first version doesn't support filled Modification and Fragment columns.
#' IMPORTANT! The kinetic data is often described as deuterium uptake curve data. 
#' We use this terms interchangeable. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{calculate_state_uptake}} 
#' \code{\link{plot_uptake_curve}}
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' calculate_kinetics(dat, 
#'                    protein = "db_CD160",
#'                    sequence = "INITSSASQEGTRLN", 
#'                    state = "CD160",
#'                    start = 1, 
#'                    end = 15,
#'                    time_0 = 0.001, 
#'                    time_100 = 1440)
#'        
#' @export calculate_kinetics

calculate_kinetics <- function(dat, 
                               protein = dat[["Protein"]][1], 
                               sequence = dat[["Sequence"]][1], 
                               state = dat[["State"]][1], 
                               start = dat[["Start"]][1], 
                               end = dat[["End"]][1],
                               time_0 = min(dat[["Exposure"]]), 
                               time_100 = max(dat["Exposure"]), 
                               deut_part = 0.9) {
  
  assert_data_frame(dat)
  assert_string(protein)
  assert_string(sequence)
  assert_number(start, lower = 0, upper = end)
  assert_number(end, lower = start)
  assert_number(time_0, lower = 0, upper = time_100)
  assert_number(time_100, lower = time_0)
  assert_number(deut_part, lower = 0, upper = 1)
  
  prep_dat <- data.table(dat)[Protein == protein & 
                                Sequence == sequence & 
                                State == state & 
                                Start == start & 
                                End == end]
  time_points <- unique(prep_dat[["Exposure"]])
  time_points_to_iterate <- time_points[time_points > time_0 & time_points < time_100]
  
  
  kin_dat <- rbindlist(lapply(time_points_to_iterate, function(time_point){
    
    uptake_dat <- calculate_state_uptake(dat = prep_dat, 
                                         protein = protein,
                                         state = state, 
                                         time_0 = time_0, 
                                         time_t = time_point, 
                                         time_100 = time_100,
                                         deut_part = deut_part)
    uptake_dat[["time_chosen"]] <- time_point
    uptake_dat
    
  }))[, .(Protein, Sequence, Start, End, State, time_chosen,
          Exposure, Modification, frac_deut_uptake, err_frac_deut_uptake,
          deut_uptake, err_deut_uptake, theo_frac_deut_uptake, 
          err_theo_frac_deut_uptake, theo_deut_uptake, err_theo_deut_uptake, 
          Med_Sequence)]
  
  kin_dat <- data.frame(kin_dat)
  
  attr(kin_dat, "protein") <- protein
  attr(kin_dat, "sequence") <- sequence
  attr(kin_dat, "state") <- state
  attr(kin_dat, "start") <- start
  attr(kin_dat, "end") <- end
  attr(kin_dat, "time_0") <- time_0
  attr(kin_dat, "time_100") <- time_100
  attr(kin_dat, "deut_part") <- deut_part
  
  return(kin_dat)
  
}