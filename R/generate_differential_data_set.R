#' generate_differential_data_set
#' 
#' @description Generate the data frame with differential data from two
#' provided states - experimental/fractional calculations with the
#' uncertainty, based on supplied parameters.
#' 
#' @importFrom tidyr gather
#' 
#' @param dat ...
#' @param states vector of two states to calculate difference between them, 
#' the order is important (state first - state second)
#' @param protein ...
#' @param time_0 ...
#' @param time_t ...
#' @param time_100 ...
#' @param deut_part ...
#' 
#' @details The names of the parameters and variables will be changed 
#' later after the glossary project.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @examples
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' # calculate differential values between states "CD160" and "CD160_HVEM" for protein "db_CD160"
#' generate_differential_data_set(dat = dat, states = c("CD160", "CD160_HVEM"), protein = "db_CD160", 
#'                                time_0 = 0.001, time_t = 5.000, time_100 = 1440.000)
#' 
#' @export generate_differential_data_set

generate_differential_data_set <- function(dat,
                                           states,
                                           protein,
                                           time_0,
                                           time_t,
                                           time_100,
                                           deut_part = 1){
  
  bind_rows(lapply(states, function(i) calculate_state_deuteration(dat, 
                                                                   protein = protein, 
                                                                   state = i, 
                                                                   time_0 = time_0,
                                                                   time_t = time_t, 
                                                                   time_100 = time_100,
                                                                   deut_part = deut_part))) %>%
    droplevels() %>% 
    mutate(State = factor(State, levels = states, labels = c("1", "2"))) %>%
    gather(variable, value, -c(Protein:End, State, Med_Sequence)) %>%
    unite(tmp, variable, State) %>%
    spread(tmp, value)  %>%
    mutate(diff_frac_deut_uptake = frac_deut_uptake_1 - frac_deut_uptake_2,
           err_diff_frac_deut_uptake = sqrt(err_frac_deut_uptake_1^2 + err_frac_deut_uptake_2^2),
           diff_deut_uptake = deut_uptake_1 - deut_uptake_2,
           err_diff_deut_uptake = sqrt(err_deut_uptake_1^2 + err_deut_uptake_2^2),
           diff_theo_frac_deut_uptake = theo_frac_deut_uptake_1 - theo_frac_deut_uptake_2, 
           err_diff_theo_frac_deut_uptake = sqrt(err_theo_frac_deut_uptake_1^2 + err_theo_frac_deut_uptake_2^2),
           diff_theo_deut_uptake = theo_deut_uptake_1 - theo_deut_uptake_2,
           err_diff_theo_deut_uptake = sqrt(err_theo_deut_uptake_1^2 + err_theo_deut_uptake_2^2)) %>%
    arrange(Start, End) %>%
    select(Protein, Start, End, Med_Sequence, everything(), -contains("1"), -contains("2"))
}