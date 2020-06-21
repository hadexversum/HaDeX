#' Experiment quality control
#' 
#' @description Checks how the uncertainty changes in a function of `out_time`.
#' 
#' @importFrom dplyr bind_rows
#' 
#' @param dat data read by \code{\link{read_hdx}}
#' @param state_first state of the first peptide
#' @param state_second state of the second peptide
#' @param chosen_time chosen time point
#' @param in_time `in` time
#' 
#' @details The function calculates mean uncertainty of all peptides and its uncertainty (standard error) based on given `in_time` and `chosen_time` 
#' as a function of `out_time`. Both theoretical and experimental results for each state and their difference are supplied for comparison but only 
#' experimental calculations depends on `out_time` variable. The results are either in form of relative or absolute values depending on the `relative` 
#' parameter supplied by the user. 
#' This data can be useful for general overview of the experiment and analyse of the chosen time parameters. 
#' 
#' @return \code{data.frame} with mean uncertainty per different `out_time` value  
#' 
#' @seealso \code{\link{read_hdx}}
#' 
#' @examples 
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' # calculate mean uncertainty 
#' (result <- quality_control(dat = dat,
#'                            state_first = "CD160",
#'                            state_second = "CD160_HVEM", 
#'                            chosen_time = 1, 
#'                            in_time = 0.001))    
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
                            state_first,
                            state_second, 
                            chosen_time, 
                            in_time){
  
  
  times <- unique(dat[["Exposure"]][dat[["Exposure"]] > chosen_time])
  
  result <- lapply(times, function(t){
    
    tmp <- prepare_dataset(dat,
                           in_state_first = paste0(state_first, "_", in_time),
                           chosen_state_first = paste0(state_first, "_", chosen_time),
                           out_state_first = paste0(state_first, "_", t),
                           in_state_second = paste0(state_second, "_", in_time),
                           chosen_state_second = paste0(state_second, "_", chosen_time),
                           out_state_second = paste0(state_second, "_", t))
    
    data.frame(t,
               mean(tmp[["err_frac_exch_state_1"]], na.rm = TRUE),
               sd(tmp[["err_frac_exch_state_1"]], na.rm = TRUE),
               mean(tmp[["err_frac_exch_state_2"]], na.rm = TRUE),
               sd(tmp[["err_frac_exch_state_2"]], na.rm = TRUE),
               mean(tmp[["err_frac_exch"]], na.rm = TRUE),
               sd(tmp[["err_frac_exch"]], na.rm = TRUE))
    
  })
  
  result <- bind_rows(result)
  
  colnames(result) <- c("out_time", "avg_err_state_first", "sd_err_state_first", "avg_err_state_second", 
                        "sd_err_state_second", "avg_diff", "sd_diff")
  
  result
  
}
