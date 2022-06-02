#' Plot peptide mass measurement
#' 
#' @description Plot the mass measurements
#' from replicates for peptide in specific time point. 
#' 
#' @importFrom ggplot2 geom_vline
#' 
#' @param dat data produced by 
#' \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state biological state for chosen protein.
#' @param sequence sequence of chosen peptide.
#' @param show_charge_values ...
#' @param time_t time point of the measurement.
#'
#' @details This function shows the measurements of mass from
#' different replicates for specific peptide in specific state
#' in specific time point of measurement on the plot. 
#' Moreover, on the plot is shown the average mass from the 
#' replicates, used later in calculations. 
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_exp_masses_per_replicate}}
#' \code{\link{calculate_exp_masses}}
#' \code{\link{calculate_state_uptake}}
#' \code{\link{calculate_diff_uptake}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' plot_peptide_mass_measurement(dat, sequence = "LICTVWHKKEEAEG")
#' plot_peptide_mass_measurement(dat, sequence = "LICTVWHKKEEAEG", show_charge_values = F)
#' 
#' @export plot_peptide_mass_measurement

plot_peptide_mass_measurement <- function(dat,
                                          protein = dat[["Protein"]][1],
                                          state = dat[["State"]][1],
                                          sequence = dat[["Sequence"]][1],
                                          show_charge_values = TRUE,
                                          time_t = unique(dat[["Exposure"]])[3]){
  
  rep_mass_dat <- calculate_exp_masses_per_replicate(dat) %>%
    filter(Protein == protein,
           State == state,
           Sequence == sequence,
           Exposure == time_t)
  
  avg_value <- mean(rep_mass_dat[["avg_exp_mass"]])
  
  if(show_charge_values){
    
    rep_mass_z_dat <- dat %>%
      filter(Protein == protein,
             State == state,
             Sequence == sequence,
             Exposure == time_t) %>%
      mutate(exp_mass = Center*z - z*1.00727647,
             weighted_Inten = scale(Inten))
    ggplot() +
      geom_point(data = rep_mass_z_dat, aes(x = exp_mass, y = File, color = as.factor(z), size = weighted_Inten)) +
      geom_point(data = rep_mass_dat, aes(x = avg_exp_mass, y = File), size = 3) +
      geom_vline(xintercept = avg_value, color = "red", linetype = "dashed", size = 1.5) +
      labs(y = "",
           x = "Measured mass [Da]",
           title = paste0("Peptide ", sequence, " in state ", state, " in ", time_t, " min"),
           color = "Charge",
           size = "rel Inten") +
      theme(legend.direction = "vertical")
  
  } else {
      
    ggplot() +
      geom_point(data = rep_mass_dat, aes(x = avg_exp_mass, y = File), size = 3) +
      geom_vline(xintercept = avg_value, color = "red", linetype = "dashed", size = 1.5) +
      labs(y = "",
           x = "Measured mass [Da]",
           title = paste0("Peptide ", sequence, " in state ", state, " in ", time_t, " min"))
        }
}


#' Show peptide mass measurement
#' 
#' @description Show the mass measurements
#' from replicates for peptide in specific time point. 
#' 
#' @param rep_mass_dat data produced by 
#' \code{\link{calculate_exp_masses_per_replicate}} function.
#' @param protein chosen protein. 
#' @param state biological state for chosen protein.
#' @param sequence sequence of chosen peptide.
#' @param time_t time point of the measurement.
#'
#' @details This function shows the measurements of mass from
#' different replicates for specific peptide in specific state
#' in specific time point of measurement. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_exp_masses_per_replicate}}
#' \code{\link{calculate_exp_masses}}
#' \code{\link{calculate_state_uptake}}
#' \code{\link{calculate_diff_uptake}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' rep_mass_dat <- calculate_exp_masses_per_replicate(dat)
#' show_peptide_mass_measurement(rep_mass_dat)
#' 
#' @export show_peptide_mass_measurement

show_peptide_mass_measurement <- function(rep_mass_dat,
                                          protein = rep_mass_dat[["Protein"]][1],
                                          state = rep_mass_dat[["State"]][1],
                                          sequence = rep_mass_dat[["Sequence"]][1],
                                          time_t = unique(rep_mass_dat[["Exposure"]])[3]){
  
  rep_mass_dat %>%
    filter(Protein == protein,
           State == state,
           Sequence == sequence,
           Exposure == time_t) %>%
    mutate(avg_exp_mass = round(avg_exp_mass, 4)) %>%
    select(Protein, Sequence, Start, End, Exposure, State, File, avg_exp_mass) %>%
    rename(`Mass` = avg_exp_mass)
  
}


#' Plot peptide charge measurement
#' 
#' @description Plot the charge measurements
#' from replicates for peptide in specific time point. 
#' 
#' @importFrom ggplot2 geom_histogram scale_x_continuous
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state biological state for chosen protein.
#' @param sequence sequence of chosen peptide.
#' @param time_t time point of the measurement.
#'
#' @details This function shows the measurements of charge from
#' different replicates for specific peptide in specific state
#' in specific time point of measurement on the plot.
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{show_peptide_charge_measurement}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' plot_peptide_charge_measurement(dat)
#' 
#' @export  plot_peptide_charge_measurement

plot_peptide_charge_measurement <- function(dat, 
                                            protein = dat[["Protein"]][1],
                                            state = dat[["State"]][1],
                                            sequence = dat[["Sequence"]][1],
                                            time_t = unique(dat[["Exposure"]])[3]){
  
  tmp_dat <- dat %>%
    filter(Protein == protein,
           State == state,
           Sequence == sequence,
           Exposure == time_t)
  
  n_bins <- length(unique(tmp_dat[["z"]]))
  min_z <- min(tmp_dat[["z"]])
  max_z <- max(tmp_dat[["z"]])
  
  tmp_dat %>%
    ggplot(aes(x = z)) +
    geom_histogram(aes(fill = File), bins = n_bins) + 
    scale_x_continuous(breaks = c(min_z:max_z)) +
    labs(title = paste0("Peptide ", sequence, " in state ", state, " in ", time_t, " min"))
  
}

#' Show peptide charge measurement
#' 
#' @description Show the charge measurements
#' from replicates for peptide in specific time point. 
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state biological state for chosen protein.
#' @param sequence sequence of chosen peptide.
#' @param time_t time point of the measurement.
#'
#' @details This function shows the measurements of charge from
#' different replicates for specific peptide in specific state
#' in specific time point of measurement.
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{plot_peptide_charge_measurement}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' show_peptide_charge_measurement(dat)
#' 
#' @export show_peptide_charge_measurement

show_peptide_charge_measurement <- function(dat, 
                                            protein = dat[["Protein"]][1],
                                            state = dat[["State"]][1],
                                            sequence = dat[["Sequence"]][1],
                                            time_t = unique(dat[["Exposure"]])[3]){
  
  dat %>%
    filter(Protein == protein,
           State == state,
           Sequence == sequence,
           Exposure == time_t) %>%
    select(Protein, Sequence, Start, End, State, Exposure, File, z) %>%
    arrange(z, File)
  
}




#' Create replicates data
#'
#' @description Create replicate data set suitable
#' for replicate histogram, for one or multiple 
#' time points of measurement. 
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function.
#' @param time_t optional, for only one time point of
#' measurement. If value is NULL, all time point from
#' \code{dat} are preserved.
#' @param protein chosen protein. 
#' @param state biological state for chosen protein.
#'
#' @details The function \code{\link{create_replicate_dataset}}
#' creates a dataset with information about how many
#' replicates are for peptides in specific state in 
#' time points of measurement. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{plot_replicate_histogram}}
#' \code{\link{show_replicate_histogram_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' create_replicate_dataset(dat)
#'
#' @export create_replicate_dataset

create_replicate_dataset <- function(dat, 
                                     time_t = NULL, 
                                     protein = unique(dat[["Protein"]])[1], 
                                     state = dat[["State"]][1]){
  
  res <- dat %>%
    calculate_exp_masses_per_replicate() %>%
    group_by(Start, End) %>%
    arrange(Start, End) %>%
    mutate(ID = cur_group_id()) %>%
    filter(if (is.null(time_t)) Exposure < 99999 else Exposure == time_t) %>%
    filter(Protein == protein,
           State == state) %>%
    select(ID, Sequence, Exposure, Start, End) %>%
    group_by(Sequence, Exposure, Start, End, ID) %>%
    summarize(n = n()) %>%
    ungroup(.) %>%
    arrange(Start, End, Exposure)
  
  attr(res, "state") <- state
  
  res
}

#' Plot replicates histogram
#' 
#' @description Plot histogram on number of replicates per
#' peptide in one or multiple time point of measurement.
#' 
#' @importFrom ggplot2 aes_string
#' 
#' @param rep_dat replicate data, created by 
#' \code{\link{create_replicate_dataset}} function.
#' @param time_points \code{logical}, indicator if the histogram
#' should show values aggregated for time points of measurements.
#'
#' @details The function shows three versions of replicate 
#' histogram, based on supplied \code{rep_dat} and \code{time_points}. 
#' If \code{time_points} is selected, the histogram shows the number 
#' of replicates for time points of measurement, to easly spot
#' if there were troubles with samples for specific time point of
#' measurement. Then, on the X-axis is Exposure (in minutes) and 
#' on the Y-axis number of replicates.
#' If \code{time_points} is not selected, onn the X-axis there are 
#' peptide ID, and on the Y-axis there are numbers of replicates. 
#' If \code{rep_dat} contains data from one time point 
#' of measurement, the histogram colors reflect the 
#' number of replicates to highlight the outliers.
#' If \code{rep_dat} contains multiple time point of
#' measurement, the colors help to distinguish between 
#' them.   
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso
#' \code{\link{create_replicate_dataset}} 
#' \code{\link{show_replicate_histogram_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' rep_dat <- create_replicate_dataset(dat)
#' plot_replicate_histogram(rep_dat)
#' 
#' plot_replicate_histogram(rep_dat, time_points = T)
#' 
#' rep_dat <- create_replicate_dataset(dat, time_t = 0.167)
#' plot_replicate_histogram(rep_dat)
#' 
#' @export plot_replicate_histogram 

plot_replicate_histogram <- function(rep_dat,
                                     time_points = F){
  
  state <- attr(rep_dat, "state")
  
  if(time_points){
    
    x <- "as.factor(Exposure)"
    fill <- "ID"
    legend_position <- "None"
    plot_title <- "Number of replicates for time points"
    plot_x <- "Exposure [min]"
    
  } else {
    
    if (length(unique(rep_dat[["Exposure"]])) == 1) {
      
      x = "ID"
      fill <- "n"
      legend_position <- "none"
      
      time_t <- unique(rep_dat[["Exposure"]])
      plot_title <- paste0("Number of replicates for each peptide in ", state, " state in ", time_t, " min")
      plot_x <- "Peptide ID"
      
    } else {
      
      x <- "ID"
      fill <- "as.factor(Exposure)"
      legend_position <- "bottom"
      
      plot_title <- paste0("Number of replicates for each peptide in ", state, " state")
      plot_x <- "Peptide ID"
      
    } 
    
    
  }
  
  ggplot(rep_dat) +
    geom_col(aes_string(x = x, y = "n", fill = fill)) +
    labs(title = plot_title,
         x = plot_x,
         y = "Number of replicates",
         fill = "Exposure") +
    theme(legend.position = legend_position)
  
}

#' Show replicate data
#' 
#' @description Show histogram data on number of replicates per
#' peptide in one or multiple time point of measurement.
#' 
#' @param rep_dat replicate data, created by 
#' \code{\link{create_replicate_dataset}} function.
#' 
#' @details The function shows the information about
#' number of replicates for peptides in one or multiple
#' time point of measurement, depends on supplied data.
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso
#' \code{\link{create_replicate_dataset}}
#' \code{\link{plot_replicate_histogram}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' rep_dat <- create_replicate_dataset(dat)
#' show_replicate_histogram_data(rep_dat)
#' 
#' @export show_replicate_histogram_data

show_replicate_histogram_data <- function(rep_dat){
  
  rep_dat %>%
    arrange(Start, End, Exposure) %>%
    select(ID, Sequence, Start, End, Exposure, n)
  
}