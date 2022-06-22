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

#' Plot kinetics data
#' 
#' @description Plots kinetics of the hydrogen-deuterium exchange for specific peptides. 
#' 
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_ribbon geom_line scale_y_continuous scale_x_log10
#' 
#' @param kin_dat calculated kinetic data by \code{\link{calculate_kinetics}} 
#' or \code{\link{calculate_peptide_kinetics}} or \code{\link{create_kinetic_dataset}}
#' function.
#' @param theoretical \code{logical}, determines if plot shows theoretical values.
#' @param fractional \code{logical}, determines if plot shows fractional values.
#' @param uncertainty_type type of presenting uncertainty, possible values:
#' "ribbon", "bars" or "bars + line".
#' @param log_x \code{logical}, determines if x axis shows logarithmic values.
#' 
#' @details This function visualizes the output of the  
#' \code{\link{calculate_kinetics}} function. 
#' Based on supplied parameters appropriate columns are chosen for the plot. 
#' The uncertainty associated with each peptide is shown as a ribbon. 
#' Axis are labeled according to the supplied parameters but no title is provided.
#' 
#' If you want to plot data for more then one peptide in one state, join 
#' calculated data by using \code{\link{bind_rows}} from dplyr package and 
#' pass the result as kin_dat.
#' 
#' @return a \code{\link{ggplot2}} object.
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
#' plot_kinetics(kin_dat = kin1, 
#'               theoretical = FALSE, 
#'               fractional = TRUE)
#' 
#' # one peptide in all states         
#' kin2 <- calculate_peptide_kinetics(dat, 
#'                                    protein = "db_CD160",
#'                                    sequence = "INITSSASQEGTRLN", 
#'                                    states = c("CD160", "CD160_HVEM"),
#'                                    start = 1, 
#'                                    end = 15,
#'                                    time_0 = 0.001, 
#'                                    time_100 = 1440)
#' plot_kinetics(kin_dat = kin2, 
#'               theoretical = FALSE, 
#'               fractional = TRUE)
#'                 
#' @export plot_kinetics

plot_kinetics <- function(kin_dat, 
                          theoretical = FALSE, 
                          fractional = FALSE,
                          uncertainty_type = "ribbon",
                          log_x = TRUE){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars", "bars + line"))
  
  if (theoretical){
    
    title <- "Theoretical uptake curve"
    
    if (fractional){
      
      value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake [%]"
      
    } else {
      
      value <- "theo_deut_uptake"
      err_value <- "err_theo_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      
    }
    
  } else {
    
    title <- "Uptake curve"
    
    if (fractional){
      
      value <- "frac_deut_uptake"
      err_value <- "err_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake [%]"
      
    } else {
      
      value <- "deut_uptake"
      err_value <- "err_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      
    }
    
  }
  
  plot_dat <- data.frame(Sequence = kin_dat[["Sequence"]],
                         Start = kin_dat[["Start"]],
                         End = kin_dat[["End"]],
                         State = kin_dat[["State"]],
                         time_chosen = kin_dat[["time_chosen"]],
                         value = kin_dat[[value]],
                         err_value = kin_dat[[err_value]],
                         prop = paste0(kin_dat[["Sequence"]], "-", kin_dat[["State"]]))
  
  kin_plot <- ggplot(plot_dat, aes(x = time_chosen, y = value, group = prop)) +
    geom_point(aes(color = prop), size = 2) + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Time points [min]", 
         y = y_label,
         title = title)
  
  if(log_x){ err_width = 0.1 } else { err_width = 2 }
  
  if(uncertainty_type == "ribbon"){
    
    kin_plot <- kin_plot +
      geom_ribbon(aes(ymin = value - err_value, ymax = value + err_value, fill = prop), alpha = 0.15) +
      geom_line(aes(color = prop)) 
    
  } else if (uncertainty_type == "bars") {
    
    kin_plot <- kin_plot +
      geom_errorbar(aes(x = time_chosen, ymin = value - err_value, ymax = value + err_value, color = prop),
                    width = err_width)
    
  } else if (uncertainty_type == "bars + line"){
    
    kin_plot <- kin_plot +
      geom_errorbar(aes(x = time_chosen, ymin = value - err_value, ymax = value + err_value, color = prop),
                    width = err_width) + 
      geom_line(aes(color = prop))
    
  }
  
  if(log_x){
    
    kin_plot <- kin_plot + 
      scale_x_log10()
    
  }
  
  return(HaDeXify(kin_plot))
}

