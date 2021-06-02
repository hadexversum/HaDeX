#' Calculate kinetics data for a peptide in specific state
#' 
#' @description Calculate kinetics of the hydrogen-deuteration exchange 
#' for given peptide.
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
#' @return a \code{\link{data.frame} object.
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
                               sequence, 
                               state, 
                               start, 
                               end,
                               time_0, 
                               time_100, 
                               deut_part = 0.9) {
  
  assert_data_frame(dat)
  assert_string(protein)
  assert_string(sequence)
  assert_number(start, lower = 0, upper = end)
  assert_number(end, lower = start)
  assert_number(time_0, lower = 0, upper = time_100)
  assert_number(time_100, lower = time_0)
  assert_number(deut_part, lower = 0, upper = 1)
  
  prep_dat <- dat %>%
    filter(Protein == protein,
           Sequence == sequence, 
           State == state,
           Start == start, 
           End == end)
  
  time_points <- unique(prep_dat[["Exposure"]])
  
  time_points_to_iterate <- time_points[time_points > time_0 & time_points < time_100]
  
  bind_rows(lapply(time_points_to_iterate, function(time_point){
    
    calculate_state_uptake(dat = prep_dat, 
                           protein = protein,
                           state = state, 
                           time_0 = time_0, 
                           time_t = time_point, 
                           time_100 = time_100,
                           deut_part = deut_part) %>%
      mutate(time_chosen = time_point) 
    
  })) %>%
    select(Protein, Sequence, Start, End, State, time_chosen, everything())
  
}

#' Calculate kinetics data for a peptide for a set of states
#' 
#' @description ...
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
                                       protein,
                                       sequence,
                                       states, 
                                       start, 
                                       end, 
                                       time_0,
                                       time_100,
                                       deut_part = 0.9){
  
  lapply(states, function(state){
    
    calculate_kinetics(dat = dat,
                       protein = protein, 
                       sequence = sequence, 
                       state = state,
                       start = start,
                       end = end, 
                       time_0 = time_0,
                       time_100 = time_100,
                       deut_part = deut_part)
    
  }) %>% bind_rows()
  
}


#' generate_kinetic_data_set
#' 
#' @description Generates the data set of deuterium uptake between selected 
#' time points based on supplied peptide list.
#' 
#' @param dat dat data imported by the \code{\link{read_hdx}} function.
#' @param peptide_list ...
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
#' @examples ...
#' 
#' @export create_kinetic_dataset

create_kinetic_dataset <- function(dat,
                                   peptide_list,
                                   protein,
                                   time_0,
                                   time_100,
                                   deut_part){
  
  bind_rows(apply(peptide_list, 1, function(peptide){
    calculate_kinetics(dat = dat,
                       protein = protein, 
                       sequence = peptide[1],
                       state = peptide[2],
                       start = as.numeric(peptide[3]),
                       end = as.numeric(peptide[4]),
                       time_0 = time_0,
                       time_100 = time_100,
                       deut_part = 0.01*as.integer(deut_part))
  }))
  
}


#' generate_kinetic_data
#' 
#' @description Generates deuterium uptake data, based on the supplied
#' parameters.
#' 
#' @param dat custom format, produced by 
#' \code{\link{generate_kinetic_data_set}}
#' @param theoretical \code{logical}, determines if plot shows theoretical values
#' @param fractional \code{logical}, determines if plot shows fractional values
#' 
#' @details This data is available in the GUI. 
#' All of the numerical values are rounded to 4 places after the dot!!
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export show_kinetic_data

show_kinetic_data <- function(dat, 
                              theoretical = FALSE, 
                              fractional = FALSE){
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional  
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, theo_frac_deut_uptake, err_avg_theo_in_time) %>%
        mutate(theo_frac_deut_uptake = round(theo_frac_deut_uptake, 4), 
               err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4)) %>%
        rename("Time Point" = time_chosen,
               "Theo Frac Exch" = theo_frac_deut_uptake,
               "Theo Err Frac Exch" = err_theo_frac_deut_uptake)
      
    } else {
      # theoretical & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, theo_deut_uptake, err_theo_deut_uptake) %>%
        mutate(theo_deut_uptake = round(theo_deut_uptake, 4), 
               err_theo_deut_uptake = round(err_theo_deut_uptake, 4)) %>%
        rename("Time Point" = time_chosen,
               "Theo Abs Val Exch" = theo_deut_uptake,
               "Theo Err Abs Val Exch" = err_theo_deut_uptake)
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, frac_deut_uptake, err_frac_deut_uptake) %>%
        mutate(frac_deut_uptake = round(frac_deut_uptake, 4), 
               err_frac_deut_uptake = round(err_frac_deut_uptake, 4)) %>%
        rename("Time Point" = time_chosen,
               "Frac Exch" = frac_deut_uptake,
               "Err Frac Exch" = err_frac_deut_uptake)
      
    } else {
      # experimental & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, time_chosen, deut_uptake, err_deut_uptake) %>%
        mutate(deut_uptake = round(deut_uptake, 4), 
               err_deut_uptake = round(err_deut_uptake, 4)) %>%
        rename("Time Point" = time_chosen,
               "Abs Val Exch" = deut_uptake,
               "Err Abs Val Exch" = err_deut_uptake)
    }
    
  }
  
}

#' Plot kinetics data
#' 
#' @description Plots kinetics of the hydrogen-deuterium exchange for specific peptides. 
#' 
#' @importFrom dplyr %>% mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_ribbon geom_line scale_y_continuous
#' 
#' @param kin_dat calculated kinetic data by \code{\link{calculate_kinetics}} 
#' function
#' @param theoretical \code{logical}, determines if plot shows theoretical values
#' @param fractional \code{logical}, determines if plot shows fractional values
#' @param uncertainty_type type of presenting uncertainty, possible values:
#' "ribbon", "bars" or "bars + line".
#' @param log_x \code{logical}, determines if x axis shows logarithmic values.
#' 
#' @seealso \code{\link{calculate_kinetics}}
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
#' @return a \code{\link[ggplot2]{ggplot}} object.
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'
#' kin1 <- calculate_kinetics(dat, 
#'                            protein = "db_CD160",
#'                            sequence = "INITSSASQEGTRLN", 
#'                            state = "CD160",
#'                            start = 1, 
#'                            end = 15,
#'                            time_in = 0.001, 
#'                            time_out = 1440)
#'   
#' plot_kinetics(kin_dat = kin1, 
#'               theoretical = TRUE, 
#'               fractional = TRUE)
#'                 
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
  
  kin_plot <- plot_dat %>% 
    ggplot(aes(x = time_chosen, y = value, group = prop)) +
    geom_point(aes(color = prop)) + 
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    scale_y_continuous(limits = c(0, NA)) + 
    labs(x = "Time points [min]", 
         y = y_label,
         title = title)
  
  if(uncertainty_type == "ribbon"){
    
    kin_plot <- kin_plot +
      geom_ribbon(aes(ymin = value - err_value, ymax = value + err_value, fill = prop), alpha = 0.15) +
      geom_line(aes(color = prop)) 
    
  } else if (uncertainty_type == "bars") {
    
    kin_plot <- kin_plot +
      geom_errorbar(aes(x = time_chosen, ymin = value - err_value, ymax = value + err_value, color = prop))
    
  } else if (uncertainty_type == "bars + line"){
    
    kin_plot <- kin_plot +
      geom_errorbar(aes(x = time_chosen, ymin = value - err_value, ymax = value + err_value, color = prop)) + 
      geom_line(aes(color = prop))
    
  }
  
  if(log_x){
    
    kin_plot <- kin_plot + 
      scale_x_log10()
    
  }
  
  kin_plot
}