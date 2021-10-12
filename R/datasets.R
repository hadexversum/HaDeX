#' Creates comparison uptake dataset
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states vector of states (for chosen protein), for which the 
#' calculations are done. 
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_t time point of the measurement for which the calculations
#' are done. 
#' @param time_100 maximal exchange control time point of measurement.
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details Function \code{\link{create_state_comparison_dataset}} is a 
#' wrapper for \code{\link{calculate_state_uptake}} function, calls 
#' this function for all (default) or chosen states in states vector.
#' 
#' @return a \code{\link{data.frame}} object. 
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' comparison_dat <- create_state_comparison_dataset(dat)
#' head(comparison_dat)
#'
#' @export create_state_comparison_dataset

create_state_comparison_dataset <- function(dat,
                                            protein = unique(dat[["Protein"]])[1],
                                            states = unique(dat[["State"]]),
                                            time_0 = 0.001,
                                            time_t = 1,
                                            time_100 = 1440,
                                            deut_part = 0.9){
  
  
  lapply(states, function(state){
    
    calculate_state_uptake(dat,
                           protein = protein,
                           state = state,
                           time_0 = time_0,
                           time_t = time_t,
                           time_100 = time_100,
                           deut_part = deut_part)
    
  }) %>% bind_rows
  
}



#' Create dataset with control
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param control_protein maximal exchange control protein, from dat. 
#' @param control_state maximal exchange control state, from dat.
#' @param control_exposure maximal exchange control exposure (time
#' point of measurement), from dat.
#' 
#' @details Function \code{\link{create_control_dataset}}
#' creates a dataset (similar to the output of \code{\link{read_hdx}} 
#' function), with maximal exchange control for all the states,
#' based on provided parameters. The other functions are operating 
#' within a state, so the control is prepared for each state. 
#' The chosen maximal exchange control is distinguishable by the value 
#' `99999` in `Exposure` control. 
#' 
#' @return a \code{\link{data.frame}} object. 
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' create_control_dataset(dat)
#' 
#' @export create_control_dataset

create_control_dataset <- function(dat,
                                   control_protein = dat[["Protein"]][1],
                                   control_state = dat[["State"]][1],
                                   control_exposure = max(dat[["Exposure"]])){
  
  tmp <- dat %>%
    filter(Protein == control_protein, 
           State == control_state, 
           Exposure == control_exposure) %>%
    mutate(Exposure = 99999)
  
  states_to_prepare <- unique(filter(dat, Protein == control_protein)[["State"]])
  
  bind_rows(dat, 
            lapply(states_to_prepare, function(state){
              peps <- dat %>%
                filter(State == state) %>%
                select(Sequence) %>%
                unique(.) %>%
                unlist(.)
              tmp %>%
                filter(Sequence %in% peps) %>%
                mutate(State = state) 
            }))
  
}

#' Calculate differential uptake 
#' 
#' @importFrom tidyr gather
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states vector of two states for chosen protein. Order is important, as the 
#' deuterium uptake difference is calculated as state_1 - state_2.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_t time point of the measurement for which the calculations
#' are done. 
#' @param time_100 maximal exchange control time point of measurement.
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
                                   time_0 = 0.001,
                                   time_t = 1,
                                   time_100 = 1440,
                                   deut_part = 0.9){
  
  bind_rows(lapply(states, function(i) calculate_state_uptake(dat, 
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
}

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
#' \code{\link{calculate_state_uptake}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' state_uptake_dat <- create_state_uptake_dataset(dat)
#' head(state_uptake_dat)
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
  
  state_uptake_dat <- lapply(times, function(time){
    
    calculate_state_uptake(dat, protein = protein, state = state,
                           time_0 = time_0, time_t = time, time_100 = time_100, 
                           deut_part = deut_part) %>%
      arrange(Start, End) %>%
      mutate( # ID = 1L:nrow(.),
        Exposure = time) %>%
      select(ID, Exposure, everything()) 
    
  }) %>% bind_rows()
  
  return(state_uptake_dat)
  
}


#' Create uptake dataset for multiple states
#'
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param states list of biological states for chosen protein.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement. 
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details Function \code{\link{create_uptake_dataset}} generates 
#' a dataset with deuterium uptake values in different forms. For each
#' peptide in chosen protein in chosen states for time points of measurement
#' between minimal and maximal control time points of measurement deuterium 
#' uptake, fractional deuterium uptake with respect to controls or theoretical
#' tabular values are calculated, with combined and propagated uncertainty. 
#' Each peptide has an ID, based on its start position. 
#' This function is a wrapper for \code{\link{create_state_uptake_dataset}}
#' but for multiple states. 
#' The output of this function can be presented in a form of 
#' comparison plot.
#'
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_uptake}}
#' \code{\link{create_state_uptake_dataset}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' uptake_dat <- create_uptake_dataset(dat, states = c("CD160", "CD160_HVEM"))
#' head(uptake_dat)
#' 
#' @export create_uptake_dataset

create_uptake_dataset <- function(dat,
                                  protein = unique(dat[["Protein"]])[1],
                                  states = unique(dat[["State"]]),
                                  time_0 = 0.001, 
                                  time_100 = 1440,
                                  deut_part = 0.9){
  
  times <- unique(dat[["Exposure"]])
  times <- times[times > time_0 & times < time_100]
  
  
  uptake_dat <- lapply(states, function(state){
    
    lapply(times, function(time){
      
      calculate_state_uptake(dat, protein = protein, 
                             state = state,
                             time_t = time, 
                             time_0 = time_0, time_100 = time_100,
                             deut_part = deut_part)
      
    }) %>% bind_rows
    
  }) %>% bind_rows()
  
  return(uptake_dat)
  
}

#' Generate differential dataset
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state_1 biological state for chosen protein. From this state values
#' the second state values are subtracted to get the deuterium uptake difference.
#' @param state_2 biological state for chosen protein. This state values are 
#' subtracted from the first state values to get the deuterium uptake difference.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement. 
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details The function \code{\link{create_diff_uptake_dataset}} 
#' generates a dataset with differential values between given biological states 
#' (state_1 - state_2). For each peptide of chosen protein for time points of 
#' measurement between minimal and maximal control time points of measurement 
#' deuterium uptake difference, fractional deuterium uptake difference with 
#' respect to controls or theoretical tabular values are calculated, with 
#' combined and propagated uncertainty. Each peptide has an ID, based on its start
#' position.
#' Function output can be visualized as a differential (Woods) plot, butterfly
#' differential plot or chiclet differential plot. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_diff_uptake}}
#' \code{\link{plot_differential_butterfly}}
#' \code{\link{plot_differential_chiclet}}
#' \code{\link{plot_differential}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' head(diff_uptake_dat)
#' 
#' @export create_diff_uptake_dataset

create_diff_uptake_dataset <- function(dat, 
                                       protein = unique(dat[["Protein"]])[1],
                                       state_1 = unique(dat[["State"]])[1],
                                       state_2 = unique(dat[["State"]])[2], 
                                       time_0 = 0.001,
                                       time_100 = 1440,
                                       deut_part = 0.9){
  
  all_times <- unique(dat[["Exposure"]])
  times <- all_times[all_times > time_0 & all_times <= time_100]
  
  diff_uptake_dat <- lapply(times, function(time){
    
    calculate_diff_uptake(dat = dat, states = c(state_1, state_2), protein = protein, 
                          time_0 = time_0, time_t = time, time_100 = time_100, 
                          deut_part = deut_part) %>%
      arrange(Start, End) %>%
      mutate(ID = 1L:nrow(.),
             Exposure = time) %>%
      select(ID, Exposure, everything()) 
    
  }) %>% bind_rows() %>%
    ungroup(.)
  
  return(diff_uptake_dat)
  
}


#' Create volcano dataset 
#'  
#' @importFrom dplyr %>%
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state_1 biological state for chosen protein. From this state values
#' the second state values are subtracted to get the deuterium uptake difference.
#' @param state_2 biological state for chosen protein. This state values are 
#' subtracted from the first state values to get the deuterium uptake difference.
#' @param p_adjustment_method method of adjustment P-values for multiple 
#' comparisons. Possible methods: "BH" (Benjamini & Hochberg correction), 
#' "bonferroni" (Bonferroni correction) and "none" (default).
#' @param confidence_level confidence level for the t-test.
#' 
#' @details The volcano plot shows the deuterium uptake difference in time 
#' for the peptides. This function prepares data for the plot.
#' For peptides in all of the time points of measurement (except for minimal
#' and maximal exchange control) the deuterium uptake difference between state_1
#' and state_2 is calculated, with its uncertainty (combined and propagated as
#' described in `Data processing` article). For each peptide in time point the 
#' P-value is calculated using unpaired t-test - the deuterium uptake difference
#' is calculated as the difference of measured masses in a given time point for two 
#' states. The tested hypothesis is that the mean masses for states from the 
#' replicates of the experiment are similar. The P-values indicates if the null
#' hypothesis can be rejected - rejection of the hypothesis means that the 
#' difference between states is statistically significant at provided confidence
#' level. The P-values can be adjusted using the provided method.
#' This plot is visible in GUI.
#' 
#' @references Hageman, T. S. & Weis, D. D. Reliable Identification of Significant 
#' Differences in Differential Hydrogen Exchange-Mass Spectrometry Measurements 
#' Using a Hybrid Significance Testing Approach. Anal Chem 91, 8008–8016 (2019).
#' 
#' @return a \code{\link{data.frame}} object with calculated deuterium uptake difference, 
#' uncertainty, P-value and -log(P-value) for the peptides from the provided data.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_exp_masses_per_replicate}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' vol_dat <- create_volcano_dataset(dat)
#' head(vol_dat)
#' 
#' @export create_volcano_dataset

create_volcano_dataset <- function(dat,
                                   protein = unique(dat[["Protein"]])[1],
                                   state_1 = unique(dat[["State"]])[1],
                                   state_2 = unique(dat[["State"]])[2],
                                   p_adjustment_method = "none",
                                   confidence_level = 0.98){
  
  p_adjustment_method <- match.arg(p_adjustment_method, c("none", "BH", "bonferroni"))
  
  dat <- dat[dat[["Protein"]] == protein, ]
  
  proton_mass <- 1.00727647
  
  tmp_dat <- dat %>%
    calculate_exp_masses_per_replicate(.) %>%
    group_by(Sequence, Start, End, State, Exposure) %>%
    summarize(avg_mass = mean(avg_exp_mass),
              err_avg_mass = sd(avg_exp_mass)/sqrt(length(Exposure)),
              masses = list(avg_exp_mass)) %>%
    arrange(Start, End) 
  
  tmp_dat_1 <- tmp_dat %>%
    filter(State == state_1) %>%
    rename(avg_mass_1 = avg_mass,
           err_avg_mass_1 = err_avg_mass, 
           masses_1 = masses) %>%
    ungroup(.) %>%
    select(-State)
  
  tmp_dat_2 <- tmp_dat %>%
    filter(State == state_2) %>%
    rename(avg_mass_2 = avg_mass,
           err_avg_mass_2 = err_avg_mass,
           masses_2 = masses) %>%
    ungroup(.) %>%
    select(-State)
  
  vol_dat <- merge(tmp_dat_1, tmp_dat_2, by = c("Sequence", "Start", "End", "Exposure"))
  
  res_volcano <- lapply(1:nrow(vol_dat), function(i){
    
    diff_d <- vol_dat[i, "avg_mass_1"] - vol_dat[i, "avg_mass_2"]
    uncertainty <- sqrt(vol_dat[i, "err_avg_mass_1"]^2 + vol_dat[i, "err_avg_mass_2"]^2 )
    
    st_1 <- vol_dat[i, "masses_1"][[1]]
    st_2 <- vol_dat[i, "masses_2"][[1]]
    
    if(length(st_1) == 1 | all(st_1 == st_2)) {
      p_value <- -1
    } else if (length(st_2) == 1){
      p_value <- -1
    } else {
      p_value <- t.test(x = st_1, y = st_2, paired = FALSE, alternative = "two.sided", conf.level = confidence_level)$p.value
    }
    
    data.frame(Sequence = vol_dat[i, "Sequence"],
               Exposure = vol_dat[i, "Exposure"],
               D_diff = diff_d,
               P_value = p_value,
               Uncertainty = uncertainty,
               Start = vol_dat[i, "Start"],
               End = vol_dat[i, "End"])
    
  }) %>% bind_rows() %>%
    filter(P_value > 0)
  
  res_volcano[["P_value"]] <- p.adjust(res_volcano[["P_value"]], method = p_adjustment_method)
  
  res_volcano %>%
    filter(P_value!=-1) %>%
    mutate(log_p_value = -log(P_value)) %>%
    select(Sequence, Start, End, Exposure, D_diff, Uncertainty, log_p_value, P_value)
  
}