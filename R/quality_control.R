#' Experiment quality control
#' 
#' @description Checks how the uncertainty changes in a function of `out_time`.
#' 
#' @importFrom dplyr bind_rows
#' @importFrom tidyr spread unite
#' 
#' @param dat data read by \code{\link{read_hdx}}
#' @param protein ...
#' @param state_first state of the first peptide
#' @param state_second state of the second peptide
#' @param time_t chosen time point
#' @param time_0 `in` time
#' @param deut_part ...
#' 
#' @details The function calculates mean uncertainty of all peptides and its 
#' uncertainty (standard error) based on given `in_time` and `chosen_time` 
#' as a function of `out_time`. Both theoretical and experimental results for 
#' each state and their difference are supplied for comparison but only 
#' experimental calculations depends on `out_time` variable. The results are 
#' either in form of fractional or absolute values depending on the `fractional` 
#' parameter supplied by the user. 
#' This data can be useful for general overview of the experiment and analyse 
#' of the chosen time parameters. 
#' 
#' @return \code{\link{data.frame}} object with mean uncertainty per different 
#' `out_time` value. 
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
#' 
#' (qc_dat <- create_quality_control_dataset(dat = dat,
#'                                           protein = "db_CD160",
#'                                           state_first = "CD160",
#'                                           state_second = "CD160_HVEM", 
#'                                           time_t = 1, 
#'                                           time_0 = 0.001,
#'                                           deut_part = 0.9))    
#'                            
#' plot_quality_control(qc_dat)
#' 
#' @export create_quality_control_dataset

create_quality_control_dataset <- function(dat,
                                           protein,
                                           state_first,
                                           state_second, 
                                           time_t, 
                                           time_0, 
                                           deut_part = 0.9){
  
  
  times <- unique(dat[["Exposure"]][dat[["Exposure"]] > time_t])
  
  result <- lapply(times, function(t){
    
    state_dat_1 <- calculate_state_uptake(dat, 
                                          protein = protein,
                                          state = state_first,
                                          time_0 = time_0, 
                                          time_t = time_t,
                                          time_100 = t,
                                          deut_part = deut_part)
    
    state_dat_2 <- calculate_state_uptake(dat, 
                                          protein = protein,
                                          state = state_second,
                                          time_0 = time_0, 
                                          time_t = time_t,
                                          time_100 = t,
                                          deut_part = deut_part)
    
    ## functionality from generate_differential_data_set()
    diff_dat <- bind_rows(state_dat_1, state_dat_2) %>%
      droplevels() %>% 
      mutate(State = factor(State, levels = c(state_first, state_second), labels = c("1", "2"))) %>%
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
  
  colnames(result) <- c("out_time", "avg_err_state_first", "sd_err_state_first", "avg_err_state_second", 
                        "sd_err_state_second", "avg_diff", "sd_diff")
  
  result
  
}

#' generate_quality_control_plot
#' 
#' @description Generates quality control plot based on supplied data.
#' 
#' @param dat data produced by \code{\link{create_quality_control_dataset}} 
#' function, scaled if necessary.
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso 
#' \code{\link{create_quality_control_dataset}}
#' \code{\link{show_quality_control_data}} 
#' 
#' @export plot_quality_control

plot_quality_control <- function(dat){
  
  dat %>%
    gather(2:7, key = 'type', value = 'value') %>%
    filter(startsWith(type, "avg")) %>%
    ggplot(aes(x = out_time, y = value, group = type)) +
    geom_point(size = 3) +
    geom_line(aes(color = type)) +
    scale_colour_discrete(name = "Mean uncertainty of: ", labels = c("difference", "first state", "second state")) +
    scale_x_log10() + 
    labs(x = "Out time [min]",
         y = "Mean uncertainty [%]",
         title = "Quality control plot for experiment")
  
}

#' generate_quality_control_data
#' 
#' @description  Generates quality control data, based on the supplied
#' parameters.
#' 
#' @param dat data produced by \code{\link{create_quality_control_dataset}} 
#' function, scaled if necessary.
#' 
#' @details This data is available in the GUI. The names of the parameters
#' and variables will be changed later after the glossary project.
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso
#' \code{\link{create_quality_control_dataset}}
#' \code{\link{plot_quality_control}} 
#' 
#' @export show_quality_control_data

show_quality_control_data <- function(dat){
  
  dat %>%
    select(out_time, avg_err_state_first, avg_err_state_second, avg_diff) %>%
    mutate(avg_err_state_first = round(avg_err_state_first, 2),
           avg_err_state_second = round(avg_err_state_second, 2),
           avg_diff = round(avg_diff, 2)) %>%
    rename("Out time" = out_time,
           "Mean error - first state [%]" = avg_err_state_first,
           "Mean error - second state [%]" = avg_err_state_second,
           "Mean error of difference [%]" = avg_diff)
}