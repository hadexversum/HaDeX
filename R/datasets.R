#' Create uptake dataset for chosen state
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state biological state for chosen protein.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement. 
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
#' \code{\link{calculate_state_deuteration}}
#' \code{\link{generate_butterfly_plot}} 
#' \code{\link{generate_comparison_plot}}
#' \code{\link{generate_chiclet_plot}}
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
                                        time_0 = 0.001,
                                        time_100 = 1440,
                                        deut_part = 0.9){
  
  all_times <- unique(dat[["Exposure"]])
  times <- all_times[all_times > time_0 & all_times <= time_100]
  
  state_uptake_dat <- lapply(times, function(time){
    
    calculate_state_deuteration(dat, protein = protein, state = state,
                                time_0 = time_0, time_t = time, time_100 = time_100, 
                                deut_part = deut_part) %>%
      arrange(Start, End) %>%
      mutate(ID = 1L:nrow(.),
             Exposure = time) %>%
      select(ID, Exposure, everything()) 
    
  }) %>% bind_rows()
  
  return(state_uptake_dat)
  
}


#' Create uptake dataset for multiple states
#'
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states list of biological states for chosen protein.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement. 
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details Function \code{\link{create_uptake_dataset}} generates 
#' a dataset with deuterium uptake values in different forms. For each
#' peptide in chosen protein in chosen states for time points of measurement
#' between minimal and maximal control time points of measurement deuterium 
#' uptake, fractional deuterium uptake with respect to controls or theoretical
#' tabular values are calculated, with combined and propagated uncertainty. 
#' Each peptide has an ID, based on its start position. 
#' This function is a wrapper for \code{\link{create_state_uptake_dataset}}
#' but for multiple states. 
#' The output of this function can be presented in a form of 
#' comparison plot.
#'
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_deuteration}}
#' \code{\link{create_state_uptake_dataset}} 
#' \code{\link{generate_comparison_plot}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' uptake_dat <- create_uptake_dataset(dat, states = c("CD160", "CD160_HVEM"))
#' head(uptake_dat)
#' 
#' @export create_uptake_dataset

create_uptake_dataset <- function(dat,
                                  protein = unique(dat[["Protein"]])[1],
                                  states = unique(dat[["State"]]),
                                  time_0 = 0.001, 
                                  time_100 = 1440,
                                  deut_part = 0.9){
  
  uptake_dat <- lapply(states, function(state){
    
    create_state_uptake_dataset(dat, protein = protein, 
                                state = state,
                                time_0 = time_0, time_100 = time_100,
                                deut_part = deut_part)
    
  }) %>% bind_rows()
  
}

#' Generate differential dataset
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state_1 biological state for chosen protein. From this state values
#' the second state values are subtracted to get the deuterium uptake difference.
#' @param state_2 biological state for chosen protein. This state values are 
#' subtracted from the first state values to get the deuterium uptake difference.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement. 
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
#' \code{\link{generate_differential_data_set}}
#' \code{\link{plot_butterfly_differential}}
#' \code{\link{plot_chiclet_differential}}
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
                                       time_0 = 0.001,
                                       time_100 = 1440,
                                       deut_part = 0.9){
  
  all_times <- unique(dat[["Exposure"]])
  times <- all_times[all_times > time_0 & all_times <= time_100]
  
  diff_uptake_dat <- lapply(times, function(time){
    
    generate_differential_data_set(dat = dat, states = c(state_1, state_2), protein = protein, 
                                   time_0 = time_0, time_t = time, time_100 = time_100, 
                                   deut_part = deut_part) %>%
      arrange(Start, End) %>%
      mutate(ID = 1L:nrow(.),
             Exposure = time) %>%
      select(ID, Exposure, everything()) 
    
  }) %>% bind_rows() %>%
    ungroup(.)
  
  return(diff_uptake_dat)
  
}
