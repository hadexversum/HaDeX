#' quality_control
#' 
#' Checks uncertainty level (mean for all peptides in sequence) depending on chosen out value.
#' Visualization for results is provided in examples. 
#' 
#' @importFrom dplyr bind_rows
#' 
#' @param dat file supplied by user using read_hdx()
#' @param first_state first state
#' @param second_state second state
#' @param chosen_time chosen time 
#' @param in_time in time
#' 
#' @return data.frame with values - time, value, uncertainty
#' 
#' @examples 
#' result <- quality_control(dat = dat,
#'                           first_state = "CD160",
#'                           second_state = "CD160_HVEM", 
#'                           chosen_time = 1, 
#'                           in_time = 0.001)
#' 
#' ggplot(result) + 
#' geom_line(aes(x = time, y = avg_err_state_first, color = "avg_err_state_first")) +
#'   geom_line(aes(x = time, y = avg_err_state_second, color = "avg_err_state_second")) +
#'   geom_line(aes(x = time, y = avg_err_theo_state_first, color = "avg_err_theo_state_first")) +
#'   geom_line(aes(x = time, y = avg_err_theo_state_second, color = "avg_err_theo_state_second")) +
#'   geom_line(aes(x = time, y = avg_diff, color = "avg_diff")) +
#'   geom_line(aes(x = time, y = avg_theo_diff, color = "avg_theo_diff")) + 
#'   scale_x_log10() +
#'   ylim(0, 0.05) + 
#'   labs(x = "log(time) [min]", y = "Average uncertainty", title = "Uncertainty change in out time")
#' 
#' @export quality_control

quality_control <- function(dat,
                            first_state,
                            second_state, 
                            chosen_time, 
                            in_time){
  
  
  times <- unique(dat[["Exposure"]][dat["Exposure"] > in_time])
  
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
               mean(tmp[["err_avg_theo_in_time_1"]], na.rm = TRUE),
               sd(tmp[["err_avg_theo_in_time_1"]], na.rm = TRUE),
               mean(tmp[["err_avg_theo_in_time_2"]], na.rm = TRUE),
               sd(tmp[["err_avg_theo_in_time_2"]], na.rm = TRUE),
               mean(tmp[["err_frac_exch"]], na.rm = TRUE),
               sd(tmp[["err_frac_exch"]], na.rm = TRUE),
               mean(tmp[["err_diff_theo_frac_exch"]], na.rm = TRUE),
               sd(tmp[["err_diff_theo_frac_exch"]], na.rm = TRUE))
    
  })
  
  result <- bind_rows(result)
  colnames(result) <- c("time", "avg_err_state_first", "sd_err_state_first", "avg_err_state_second", "sd_err_state_second", "avg_err_theo_state_first", "sd_err_theo_state_first", "avg_err_theo_state_second", "sd_err_theo_state_second", "avg_diff", "sd_diff", "avg_theo_diff", "sd_theo_diff")

  result
  
}