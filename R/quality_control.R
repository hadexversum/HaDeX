#' Experiment quality control
#' 
#' @description Checks how the uncertainty changes in a function of maximal
#' exchange control time of measurement.
#' 
#' @importFrom dplyr bind_rows
#' @importFrom tidyr spread unite
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state_1 first biological state.
#' @param state_2 second biological state.
#' @param time_t time point of the measurement for which the calculations
#' are done. 
#' @param time_0 minimal exchange control time point of measurement.
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details The function calculates mean uncertainty of all peptides and its 
#' uncertainty (standard error) based on given `time_0` and `time_t` 
#' as a function of `time_100`. Both theoretical and experimental results for 
#' each state and their difference are supplied for comparison but only 
#' experimental calculations depends on `time_100` variable. The results are 
#' either in form of fractional or absolute values depending on the `fractional` 
#' parameter supplied by the user. 
#' This data can be useful for general overview of the experiment and analyse 
#' of the chosen time parameters. 
#' 
#' @return \code{\link{data.frame}} object with mean uncertainty per different 
#' `time_100` value. 
#' The values are shown as percentages. 
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}
#' \code{\link{plot_quality_control}}
#' \code{\link{show_quality_control_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' create_quality_control_dataset(dat)    
#'                            
#' @export create_quality_control_dataset

create_quality_control_dataset <- function(dat,
                                           protein = dat[["Protein"]][1],
                                           state_1 = unique(dat[["State"]])[1],
                                           state_2 = unique(dat[["State"]])[2], 
                                           time_t = unique(dat[["Exposure"]])[2], 
                                           time_0 = min(dat[["Exposure"]]), 
                                           deut_part = 0.9){
  
  
  times <- unique(dat[["Exposure"]][dat[["Exposure"]] > time_t])
  
  result <- lapply(times, function(t){
    
    state_dat_1 <- calculate_state_uptake(dat, 
                                          protein = protein,
                                          state = state_1,
                                          time_0 = time_0, 
                                          time_t = time_t,
                                          time_100 = t,
                                          deut_part = deut_part)
    
    state_dat_2 <- calculate_state_uptake(dat, 
                                          protein = protein,
                                          state = state_2,
                                          time_0 = time_0, 
                                          time_t = time_t,
                                          time_100 = t,
                                          deut_part = deut_part)
    
    ## functionality from generate_differential_data_set()
    diff_dat <- bind_rows(state_dat_1, state_dat_2) %>%
      droplevels() %>% 
      mutate(State = factor(State, levels = c(state_1, state_2), labels = c("1", "2"))) %>%
      gather(variable, value, -c(Protein:End, State, Med_Sequence)) %>%
      unite(tmp, variable, State) %>%
      spread(tmp, value)  %>%
      mutate(diff_frac_deut_uptake = frac_deut_uptake_1 - frac_deut_uptake_2,
             err_diff_frac_deut_uptake = sqrt(err_frac_deut_uptake_1^2 + err_frac_deut_uptake_2^2)) %>%
      arrange(Start, End) %>%
      select(diff_frac_deut_uptake, err_diff_frac_deut_uptake)
    
    data.frame(t,
               mean(state_dat_1[["err_frac_deut_uptake"]], na.rm = TRUE),
               sd(state_dat_1[["err_frac_deut_uptake"]], na.rm = TRUE),
               mean(state_dat_2[["err_frac_deut_uptake"]], na.rm = TRUE),
               sd(state_dat_2[["err_frac_deut_uptake"]], na.rm = TRUE),
               mean(diff_dat[["err_diff_frac_deut_uptake"]], na.rm = TRUE),
               sd(diff_dat[["err_diff_frac_deut_uptake"]], na.rm = TRUE))
    
  })
  
  result <- bind_rows(result)
  
  colnames(result) <- c("time_100", "avg_err_state_1", "sd_err_state_1", "avg_err_state_2", 
                        "sd_err_state_2", "avg_diff", "sd_diff")
  
  result
  
}

#' Plot quality control data
#' 
#' @description Generates quality control plot based on supplied data.
#' 
#' @importFrom ggplot2 scale_colour_discrete
#' 
#' @param qc_dat data produced by \code{\link{create_quality_control_dataset}} 
#' function, scaled if necessary.
#' 
#' @details This plot presents the mean uncertainty in function of selected 
#' maximal exchange control time of measurement. 
#' This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso 
#' \code{\link{create_quality_control_dataset}}
#' \code{\link{show_quality_control_data}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' qc_dat <- create_quality_control_dataset(dat)  
#' plot_quality_control(qc_dat)
#' 
#' @export plot_quality_control

plot_quality_control <- function(qc_dat){
  
  qc_dat %>%
    gather(2:7, key = 'type', value = 'value') %>%
    filter(startsWith(type, "avg")) %>%
    ggplot(aes(x = time_100, y = value, group = type)) +
    geom_point(size = 3) +
    geom_line(aes(color = type)) +
    scale_colour_discrete(name = "Mean uncertainty of: ", labels = c("difference", "state_1", "state_2")) +
    scale_x_log10() + 
    labs(x = "Control time [min]",
         y = "Mean uncertainty [%]",
         title = "Quality control plot for experiment")
  
}

#' Show quality control data
#' 
#' @description  Generates quality control data, based on the supplied
#' parameters.
#' 
#' @param qc_dat data produced by \code{\link{create_quality_control_dataset}} 
#' function, scaled if necessary.
#' 
#' @details This data frame presents the mean uncertainty in function of selected 
#' maximal exchange control time of measurement. 
#' This data is available in the GUI. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso
#' \code{\link{create_quality_control_dataset}}
#' \code{\link{plot_quality_control}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' qc_dat <- create_quality_control_dataset(dat)  
#' show_quality_control_data(qc_dat)
#' 
#' @export show_quality_control_data

show_quality_control_data <- function(qc_dat){
  
  qc_dat %>%
    select(time_100, avg_err_state_1, avg_err_state_2, avg_diff) %>%
    mutate(avg_err_state_1 = round(avg_err_state_1, 2),
           avg_err_state_2 = round(avg_err_state_2, 2),
           avg_diff = round(avg_diff, 2)) %>%
    rename("Out time" = time_100,
           "Mean error - state 1 [%]" = avg_err_state_1,
           "Mean error - state 2 [%]" = avg_err_state_2,
           "Mean error of difference [%]" = avg_diff)
}