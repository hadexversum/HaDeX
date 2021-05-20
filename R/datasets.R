#' Create uptake dataset for chosen state
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state biological state for chosen protein.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement. 
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details The function \code{\link{create_uptake_dataset}} generates 
#' a dataset with deuterium uptake values in different forms. For each
#' peptide in chosen protein in chosen state for time points of measurement
#' between minimal and maximal control time points of measurement deuterium 
#' uptake, fractional deuterium uptake with respect to controls or theoretical
#' tabular values are calculated, with combined and propagated uncertainty. 
#' Each peptide has an ID, based on its start position. 
#' This data can be presented in a form of comparison plot, butterfly plot or
#' chiclet plot. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_deuteration}}
#' \code{\link{generate_butterfly_plot}} 
#' \code{\link{generate_comparison_plot}}
#' \code{\link{generate_chiclet_plot}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' uptake_dat <- create_uptake_dataset(dat)
#' head(uptake_dat)
#' 
#' @export create_state_uptake_dataset

create_state_uptake_dataset <- function(dat, 
                                  protein = unique(dat[["Protein"]])[1],
                                  state = (dat[["State"]])[1], 
                                  time_0 = 0.001,
                                  time_100 = 1440,
                                  deut_part = 0.9){
  
  all_times <- unique(dat[["Exposure"]])
  times <- all_times[all_times > time_0 & all_times <= time_100]
  
  uptake_dat <- lapply(times, function(t){
    
    calculate_state_deuteration(dat, protein = protein, state = state,
                                time_0 = time_0, time_t = t, time_100 = time_100, deut_part = deut_part) %>%
      arrange(Start, End) %>%
      mutate(ID = 1L:nrow(.),
             Exposure = factor(t)) %>%
      select(ID, Exposure, everything()) 
    
  }) %>% bind_rows()
  
  return(uptake_dat)
  
}



