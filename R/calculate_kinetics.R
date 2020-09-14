#' Calculate kinetic data
#' 
#' @description Calculate kinetics of the hydrogen-deuteration exchange 
#' for given peptide.
#' 
#' @importFrom dplyr %>% bind_rows mutate everything
#' 
#' @param dat dat data read by \code{\link{read_hdx}}
#' @param protein protein value for chosen peptide
#' @param sequence sequence of the peptide for which the kinetics is calculated
#' @param state state of given sequence
#' @param start end of given sequence
#' @param end end of given sequence
#' @param time_0 time in for experimental calculations
#' @param time_100 time out for experimental calculations
#' @param deut_part percentage of deuterium the protein was exposed to, value in range [0, 1]
#' 
#' @details The function calculates deuteration data for all available data points 
#' for given peptide. 
#' All four variants (relative & theoretical combinations) of deuteration computations 
#' are supported. Manual correction of percentage of deuterium the protein was exposed 
#' to during the exchange in theoretical calculations is provided.
#' To visualize obtained data we recommend using \code{\link{plot_kinetics}} function.
#' The first version doesn't support filled Modification and Fragment columns.
#' 
#' @return data frame with deuteration calculated for all the data points 
#' between time_in and time_out. 
#' The chosen time point for which deuteration in all four variants is calculated 
#' is available in column `time_chosen`. The rest of
#' the returned structure is equivalent to structure returned by
#' \code{\link{calculate_state_deuteration}}.
#' 
#' @seealso 
#' \code{\link{read_hdx}} \code{\link{calculate_state_deuteration}} \code{\link{plot_kinetics}}
#' 
#' @examples
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' # calculate data for sequence INITSSASQEGTRLN in state CD160
#' (kin1 <- calculate_kinetics(dat, 
#'                    protein = "db_CD160",
#'                    sequence = "INITSSASQEGTRLN", 
#'                    state = "CD160",
#'                    start = 1, 
#'                    end = 15,
#'                    time_0 = 0.001, 
#'                    time_100 = 1440))
#'                    
#' # calculate data for sequence INITSSASQEGTRLN in state CD160_HVEM
#' (kin2 <- calculate_kinetics(dat, 
#'                    protein = "db_CD160",
#'                    sequence = "INITSSASQEGTRLN", 
#'                    state = "CD160_HVEM",
#'                    start = 1, 
#'                    end = 15,
#'                    time_0 = 0.001, 
#'                    time_100 = 1440))
#'                    
#' # load extra libraries
#' library(dplyr)
#' library(ggplot2)
#' 
#' # plot example - experimental and relative 
#' bind_rows(kin1, kin2) %>% 
#'  plot_kinetics(theoretical = FALSE, 
#'                fractional = TRUE) +
#'  labs(title = "Kinetic fractional plot for INITSSASQEGTRLN")
#'  
#' # plot example - theoretical and absolute
#' bind_rows(kin1, kin2) %>%
#'   plot_kinetics(theoretical = TRUE, 
#'                 fractional = FALSE) +
#'   labs(title = "Theoretical kinetics plot for INITSSASQEGTRLN")
#'        
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
                               deut_part = 1) {
  
  
  
  
  prep_dat <- dat %>%
    filter(Protein == protein,
           Sequence == sequence, 
           State == state,
           Start == start, 
           End == end)
  
  time_points <- unique(prep_dat[["Exposure"]])
  
  time_points_to_iterate <- time_points[time_points > time_0 & time_points < time_100]
  
  bind_rows(lapply(time_points_to_iterate, function(time_point){
    
    calculate_state_deuteration(dat = prep_dat, 
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