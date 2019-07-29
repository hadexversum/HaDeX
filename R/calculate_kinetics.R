#' calculate_kinetics
#' 
#' Calculate kinetics - deuteration change in time for given peptide.
#' 
#' @importFrom dplyr %>% bind_rows
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
#' calculate_kinetics(dat, 
#'                    protein = "db_CD160",
#'                    sequence = "INITSSASQEGTRLN", 
#'                    state = "CD160",
#'                    start = 1, 
#'                    end = 15,
#'                    time_in = 0.001, 
#'                    time_out = 1440)
#'
#' @seealso 
#' calculate_state_deuteration
#' 
#' @return data frame with deuteration calculated for all the data points between time_in and time_out. 
#' Chosen time point for which deuteration in all four variants is calculated is available in column `time_chosen`. The rest of
#' the returned structure is equivalent to structure returned by calculate_state_deuteration
#' 
#' @export calculate_kinetics

calculate_kinetics <- function(dat, 
                               protein, 
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
    select(Protein, Sequence, Start, End, State, time_chosen, everything())
  
}