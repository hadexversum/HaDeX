#' Calculate differential uptake 
#' 
#' @importFrom tidyr gather
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states vector of two states for chosen protein. Order is important, as the 
#' deuterium uptake difference is calculated as state_1 - state_2.
#' @param time_0 minimal exchange control time point of measurement [min].
#' @param time_t time point of the measurement for which the calculations
#' are done [min]. 
#' @param time_100 maximal exchange control time point of measurement [min].
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details Function \code{\link{calculate_diff_uptake}} calculates
#' differential values based on provided criteria for peptides for chosen
#' protein in selected states. The methods of calculation of deuterium uptake
#' difference, fractional deuterium uptake difference with respect to 
#' minimal/maximal exchange controls or theoretical tabular values are
#' thoroughly described in the `Data processing` article, as well as 
#' law of propagation of uncertainty, used to calculate uncertainty. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_dat <- calculate_diff_uptake(dat)
#' head(diff_dat)
#' 
#' @export calculate_diff_uptake

calculate_diff_uptake  <- function(dat,
                                   protein = unique(dat[["Protein"]][1]),
                                   states = unique(dat[["State"]])[1:2],
                                   time_0 = min(dat[dat[["Exposure"]]>0, ][["Exposure"]]),
                                   time_t = unique(dat[["Exposure"]])[3], 
                                   time_100 = max(dat[["Exposure"]]),
                                   deut_part = 0.9){
  
  diff_dat <- bind_rows(lapply(states, function(i) calculate_state_uptake(dat, 
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
    select(Protein, Start, End, Med_Sequence, everything(), -contains("1"), -contains("2")) %>%
    mutate(ID = 1L:nrow(.))
  
  attr(diff_dat, "protein") <- protein
  attr(diff_dat, "states") <- states
  attr(diff_dat, "time_0") <- time_0
  attr(diff_dat, "time_t") <- time_t 
  attr(diff_dat, "time_100") <- time_100
  attr(diff_dat, "deut_part") <- deut_part
  attr(diff_dat, "n_rep") <- get_n_replicates(dat)
  
  return(diff_dat)
}

