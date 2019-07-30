#' calculate_kinetics
#' 
#' Calculate kinetics - deuteration change in time for given peptide.
#' 
#' @importFrom dplyr %>% bind_rows mutate everything
#' 
#' @param dat data frame with data from Dynamix file
#' @param protein protein value for chosen peptide
#' @param sequence sequence of the peptide for which the kinetics is calculated
#' @param state state of given sequence
#' @param start end of given sequence
#' @param end end of given sequence
#' @param time_in time in for experimental calculations
#' @param time_out time out for experimental calculations
#'
#' @examples 
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' # calculate data for sequence INITSSASQEGTRLN in state CD160
#' (kin1 <- calculate_kinetics(dat, 
#'                    protein = "db_CD160",
#'                    sequence = "INITSSASQEGTRLN", 
#'                    state = "CD160",
#'                    start = 1, 
#'                    end = 15,
#'                    time_in = 0.001, 
#'                    time_out = 1440))
#'                    
#' # calculate data for sequence INITSSASQEGTRLN in state CD160_HVEM
#' (kin2 <- calculate_kinetics(dat, 
#'                    protein = "db_CD160",
#'                    sequence = "INITSSASQEGTRLN", 
#'                    state = "CD160_HVEM",
#'                    start = 1, 
#'                    end = 15,
#'                    time_in = 0.001, 
#'                    time_out = 1440))
#'                    
#' # plot data together 
#' bind_rows(kin1, kin2) %>% 
#'  mutate(time_chosen = factor(time_chosen)) %>%
#'  ggplot(aes(x = time_chosen, y = frac_exch_state, group = State)) +
#'  geom_point() + 
#'  geom_line(aes(color = State)) +
#'  labs(title = "Kinetic plot for INITSSASQEGTRLN", 
#'       x = "Time point [min]", 
#'       y = "Deuteration") +
#'  coord_cartesian(ylim = c(0, 1)) +
#'  theme(legend.position = "bottom",
#'        legend.title = element_blank())
#'        
#' @seealso 
#' calculate_state_deuteration
#' 
#' @return data frame with deuteration calculated for all the data points between time_in and time_out. 
#' Chosen time point for which deuteration in all four variants is calculated is available in column `time_chosen`. The rest of
#' the returned structure is equivalent to structure returned by calculate_state_deuteration.
#' 
#' @details First version doesn't support filled Modification and Fragment.
#' 
#' @export calculate_kinetics

calculate_kinetics <- function(dat, 
                               protein = dat[["Protein"]][1], 
                               sequence, 
                               state, 
                               start, 
                               end,
                               time_in, 
                               time_out) {
  
  prep_dat <- dat %>%
    filter(Sequence == sequence, 
           State == state,
           Start == start, 
           End == end)
  
  time_points <- unique(prep_dat[["Exposure"]])
  
  time_points_to_iterate <- time_points[time_points > time_in & time_points < time_out]
  
  bind_rows(lapply(time_points_to_iterate, function(time_point){
    
    calculate_state_deuteration(dat = prep_dat, 
                                protein = protein,
                                state = state, 
                                time_in = time_in, 
                                time_chosen = time_point, 
                                time_out = time_out) %>%
      mutate(time_chosen = time_point) 
    
  })) %>%
    select(Protein, Sequence, Start, End, State, time_chosen, everything()) %>%
    mutate(time_chosen = factor(time_chosen))
  
}