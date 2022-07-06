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
#' To visualize obtained data we recommend using \code{\link{plot_kinetics}} function.
#' The first version doesn't support filled Modification and Fragment columns.
#' IMPORTANT! The kinetic data is often described as deuterium uptake curve data. 
#' We use this terms interchangeable. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{calculate_state_uptake}} 
#' \code{\link{plot_kinetics}}
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

#' Calculate kinetics dataset 
#' 
#' @description Calculate kinetics of the hydrogen-deuteration exchange 
#' for given peptide in multiple biological states.
#' 
#' @param dat dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states vector of states (for chosen protein), for which the 
#' calculations are done. 
#' @param sequence sequence of chosen peptide.
#' @param start start position of chosen peptide.
#' @param end end position of chosen peptide.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement.
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details Function \code{\link{calculate_peptide_kinetics}} calculates
#' kinetic data for chosen peptide in chosen biological states.
#' It is a wrapper for \code{\link{calculate_kinetics}} but for mutltiple
#' states.
#' The output of this function can be visualized using \code{\link{plot_kinetics}}.
#' IMPORTANT! The kinetic data is often described as deuterium uptake curve data. 
#' We use this terms interchangeable. 
#' 
#' @seealso 
#' \code{\link{calculate_kinetics}}
#' \code{\link{calculate_state_uptake}}
#' \code{\link{plot_kinetics}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' calculate_peptide_kinetics(dat, 
#'                            protein = "db_CD160",
#'                            sequence = "INITSSASQEGTRLN", 
#'                            states = c("CD160", "CD160_HVEM"),
#'                            start = 1, 
#'                            end = 15,
#'                            time_0 = 0.001, 
#'                            time_100 = 1440)
#' 
#' @export calculate_peptide_kinetics

calculate_peptide_kinetics <- function(dat,
                                       protein = dat[["Protein"]][1],
                                       sequence = dat[["Sequence"]][1],
                                       states = unique(dat[["State"]]), 
                                       start = dat[["Start"]][1], 
                                       end = dat[["End"]][1], 
                                       time_0 = min(dat[["Exposure"]]),
                                       time_100 = max(dat[["Exposure"]]),
                                       deut_part = 0.9){
  
  dat <- data.table(dat)
  
  kin_dat <- rbindlist(lapply(states, function(state){
    
    calculate_kinetics(dat = dat,
                       protein = protein, 
                       sequence = sequence, 
                       state = state,
                       start = start,
                       end = end, 
                       time_0 = time_0,
                       time_100 = time_100,
                       deut_part = deut_part)
    
  }))
  
  kin_dat <- data.frame(kin_dat)
  
  attr(kin_dat, "protein") <- protein
  attr(kin_dat, "sequence") <- sequence
  attr(kin_dat, "states") <- states
  attr(kin_dat, "start") <- start
  attr(kin_dat, "end") <- end
  attr(kin_dat, "time_0") <- time_0
  attr(kin_dat, "time_100") <- time_100
  attr(kin_dat, "deut_part") <- deut_part
  attr(kin_dat, "has_modification") <- attr(dat, "has_modification")
  
  return(kin_dat)
  
}


#' Create kinetics dataset for a list of peptides and their states
#' 
#' @description Generates the data set of deuterium uptake between selected 
#' time points based on supplied peptide list.
#' 
#' @param dat dat data imported by the \code{\link{read_hdx}} function.
#' @param peptide_list list of peptides for the calculation.
#' @param protein chosen protein. 
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement.
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details This is a wrapper for \code{\link{calculate_kinetics}}, but for
#' the peptide list instead of one peptide. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso
#' \code{\link{calculate_kinetics}}
#' \code{\link{calculate_state_uptake}}
#' \code{\link{plot_kinetics}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' peptide_list <- data.frame(Sequence = c("INITSSASQEGTRLN", "INITSSASQEGTRLN"), state = c("CD160", "CD160_HVEM"), start = c(1, 1), end = c(15, 15))
#' create_kinetic_dataset(dat, peptide_list)
#' 
#' peptide_list2 <- data.frame(Sequence = c("INITSSASQEGTRLN", "LICTVW"), state = c("CD160", "CD160"), start = c(1, 16), end = c(15, 21))
#' create_kinetic_dataset(dat, peptide_list2)
#' 
#' @export create_kinetic_dataset

create_kinetic_dataset <- function(dat,
                                   peptide_list,
                                   protein = dat[["Protein"]][1],
                                   time_0 = min(dat[["Exposure"]]),
                                   time_100 = max(dat[["Exposure"]]),
                                   deut_part = 0.9){
  
  dat <- data.table(dat)
  
  kin_dat <- rbindlist(apply(peptide_list, 1, function(peptide){
    calculate_kinetics(dat = dat,
                       protein = protein, 
                       sequence = peptide[1],
                       state = peptide[2],
                       start = as.numeric(peptide[3]),
                       end = as.numeric(peptide[4]),
                       time_0 = time_0,
                       time_100 = time_100,
                       deut_part = deut_part)
  }))
  
  kin_dat <- data.frame(kin_dat)
  
  attr(kin_dat, "protein") <- protein
  attr(kin_dat, "peptide_list") <- peptide_list
  attr(kin_dat, "time_0") <- time_0
  attr(kin_dat, "time_100") <- time_100
  attr(kin_dat, "deut_part") <- deut_part
  attr(kin_dat, "has_modification") <- attr(dat, "has_modification")
  
  return(kin_dat)
  
}


#' Shows selected kinetics data
#' 
#' @description Generates deuterium uptake data, based on the supplied
#' parameters.
#' 
#' @param kin_dat calculated kinetic data by \code{\link{calculate_kinetics}} 
#' or \code{\link{calculate_peptide_kinetics}} or \code{\link{create_kinetic_dataset}}
#' function.
#' @param theoretical \code{logical}, determines if plot shows theoretical values.
#' @param fractional \code{logical}, determines if plot shows fractional values.
#' 
#' @details This data is available in the GUI. 
#' All of the numerical values are rounded to 4 places after the dot!!
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_kinetics}}
#' \code{\link{calculate_peptide_kinetics}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'
#' # one peptide in one state
#' kin1 <- calculate_kinetics(dat, 
#'                            protein = "db_CD160",
#'                            sequence = "INITSSASQEGTRLN", 
#'                            state = "CD160",
#'                            start = 1, 
#'                            end = 15,
#'                            time_0 = 0.001, 
#'                            time_100 = 1440)
#' show_kinetic_data(kin1)
#' 
#' @export show_kinetic_data

show_kinetic_data <- function(kin_dat, 
                              theoretical = FALSE, 
                              fractional = FALSE){
  
  kin_dat <- data.table(kin_dat)
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional
      tmp <- kin_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         theo_frac_deut_uptake, err_theo_frac_deut_uptake)]
      tmp[, `:=`(theo_frac_deut_uptake = round(theo_frac_deut_uptake, 4),
                 err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "theo_frac_deut_uptake", 
                      "err_theo_frac_deut_uptake"),
               c("Time Point", "Theo Frac DU [%]", "Theo Err Frac DU [%]"))
      
    } else {
      # theoretical & absolute
      tmp <- kin_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         theo_deut_uptake, err_theo_deut_uptake)]
      tmp[, `:=`(theo_deut_uptake = round(theo_deut_uptake, 4),
                 err_theo_deut_uptake = round(err_theo_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "theo_deut_uptake", "err_theo_deut_uptake"),
               c("Time Point", "Theo DU [Da]", "Theo Err DU [Da]"))
      
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      tmp <- kin_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         frac_deut_uptake, err_frac_deut_uptake)]
      tmp[, `:=`(frac_deut_uptake = round(frac_deut_uptake, 4),
                 err_frac_deut_uptake = round(err_frac_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "frac_deut_uptake", "err_frac_deut_uptake"),
               c("Time Point", "Frac DU [%]", "Err Frac DU [%]"))
      
    } else {
      # experimental & absolute
      tmp <- kin_dat[, .(Protein, Sequence, State, Start, End, time_chosen, 
                         deut_uptake, err_deut_uptake)]
      tmp[, `:=`(deut_uptake = round(deut_uptake, 4),
                 err_deut_uptake = round(err_deut_uptake, 4))]
      setnames(tmp, c("time_chosen", "deut_uptake", "err_deut_uptake"),
               c("Time Point", "DU [Da]", "Err DU [Da]"))
    }
  }
}