#' Experiment quality control
#' 
#' @description Checks how the uncertainty changes in a function of `out_time`.
#' 
#' @importFrom dplyr bind_rows
#' @importFrom tidyr spread
#' 
#' @param dat data read by \code{\link{read_hdx}}
#' @param protein ...
#' @param state_first state of the first peptide
#' @param state_second state of the second peptide
#' @param time_t chosen time point
#' @param time_0 `in` time
#' @param deut_part ...
#' 
#' @details The function calculates mean uncertainty of all peptides and its uncertainty (standard error) based on given `in_time` and `chosen_time` 
#' as a function of `out_time`. Both theoretical and experimental results for each state and their difference are supplied for comparison but only 
#' experimental calculations depends on `out_time` variable. The results are either in form of fractional or absolute values depending on the `fractional` 
#' parameter supplied by the user. 
#' This data can be useful for general overview of the experiment and analyse of the chosen time parameters. 
#' 
#' @return \code{data.frame} with mean uncertainty per different `out_time` value. The values are shown as percentages. 
#' 
#' @seealso \code{\link{read_hdx}}
#' 
#' @examples 
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' # calculate mean uncertainty 
#' (result <- quality_control(dat = dat,
#'                            protein = "db_CD160",
#'                            state_first = "CD160",
#'                            state_second = "CD160_HVEM", 
#'                            time_t = 1, 
#'                            time_0 = 0.001,
#'                            deut_part = 1))    
#'                            
#' # load extra libraries
#' library(ggplot2)
#' library(tidyr)
#' library(dplyr)
#' 
#' # example of data visualization 
#' gather(result, 2:7, key = 'type', value = 'value') %>%
#' filter(startsWith(type, "avg")) %>%
#'   ggplot(aes(x = factor(out_time), y = value, group = type)) +
#'   geom_line(aes(color = type)) +
#'   labs(x = "Out time", 
#'        y = "Mean uncertainty")
#' 
#' @export quality_control

quality_control <- function(dat,
                            protein,
                            state_first,
                            state_second, 
                            time_t, 
                            time_0, 
                            deut_part){
  
  
  times <- unique(dat[["Exposure"]][dat[["Exposure"]] > time_t])
  
  result <- lapply(times, function(t){
    
    state_dat_1 <- calculate_state_deuteration(dat, 
                                               protein = protein,
                                               state = state_first,
                                               time_0 = time_0, 
                                               time_t = time_t,
                                               time_100 = t,
                                               deut_part = deut_part)
    
    state_dat_2 <- calculate_state_deuteration(dat, 
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
