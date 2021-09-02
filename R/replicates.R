#' 
#' 
#' @description ...
#' 
#' @param replicate_masses_time_t ...
#'
#' @details ...
#' 
#' @return ...
#' 
#' @seealso ...
#' 
#' @examples ...
#' 
#' @export 

plot_peptide_measurement <- function(replicate_masses_time_t){
  
  avg_value <- mean(replicate_masses_time_t[["avg_exp_mass"]])
  
  ggplot(replicate_masses_time_t, aes(x = avg_exp_mass, y = File)) +
    geom_point(size = 3) +
    geom_vline(xintercept = avg_value, color = "red", linetype = "dashed", size = 1.5)
  
}


#' 
#' 
#' @description ...
#' 
#' @param replicate_masses_time_t ...
#'
#' @details ...
#' 
#' @return ...
#' 
#' @seealso ...
#' 
#' @examples ...
#' 
#' @export 

show_peptide_measurement <- function(replicate_masses_time_t){
  
  replicate_masses_time_t %>%
    mutate(avg_exp_mass = round(avg_exp_mass, 4)) %>%
    select(Protein, Sequence, Start, End, Exposure, State, File, avg_exp_mass) %>%
    rename(`Mass` = avg_exp_mass)
  
}


#' 
#' 
#' @description ...
#' 
#' @param replicates_z_values_time_t ...
#'
#' @details ...
#' 
#' @return ...
#' 
#' @seealso ...
#' 
#' @examples ...
#' 
#' @export 

plot_peptide_z <- function(replicates_z_values_time_t){
  
  n_bins <- length(unique(replicates_z_values_time_t[["z"]]))
  min_z <- min(replicates_z_values_time_t[["z"]])
  max_z <- max(replicates_z_values_time_t[["z"]])
  
  replicates_z_values_time_t %>%
    ggplot(aes(x = z)) +
    geom_histogram(aes(fill = File), bins = n_bins) + 
    scale_x_continuous(breaks = c(min_z:max_z)) 
  
}

#' 
#' 
#' @description ...
#' 
#' @param replicates_z_values_time_t ...
#'
#' @details ...
#' 
#' @return ...
#' 
#' @seealso ...
#' 
#' @examples ...
#' 
#' @export 

show_peptide_z <- function(replicates_z_values_time_t){
  
  replicates_z_values_time_t
  
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
  
  dat %>%
    calculate_exp_masses_per_replicate() %>%
    group_by(Start, End) %>%
    arrange(Start, End) %>%
    mutate(ID = cur_group_id()) %>%
    filter(if (is.null(time_t)) Exposure < 99999 else Exposure == time_t) %>%
    filter(Protein == protein,
           State == state) %>%
    select(Sequence, Exposure, Start, End, ID) %>%
    group_by(Sequence, Exposure, Start, End, ID) %>%
    summarize(n = n())
  
}

#' Plot replicates histogram
#' 
#' @description Plot histogram on number of replicates per
#' peptide in one or multiple time point of measurement.
#' 
#' @param rep_dat replicate data, created by 
#' \code{\link{create_replicate_dataset}} function.
#'
#' @details The function shows two versions of replicate 
#' histogram, based on supplied \code{rep_dat}. 
#' On the X-axis there are peptide ID, and on the Y-axis
#' there are numbers of replicates. 
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
#' rep_dat <- create_replicate_dataset(dat, time_t = 0.167)
#' plot_replicate_histogram(rep_dat)
#' 
#' @export plot_replicate_histogram 

plot_replicate_histogram <- function(rep_dat){
  
  if (length(unique(rep_dat[["Exposure"]])) == 1) {
    
    fill <- "n"
    legend_position <- "none"
    
    time_t <- unique(rep_dat[["Exposure"]])
    plot_title <- paste0("Number of replicates for each peptide in ", state, " state in ", time_t, " min")
    
    
  } else {
    
    
    fill <- "as.factor(Exposure)"
    legend_position <- "bottom"
    
    plot_title <- paste0("Number of replicates for each peptide in ", state, " state")
    
  } 
  
  ggplot(rep_dat) +
    geom_col(aes_string(x = "ID", y = "n", fill = fill)) +
    labs(title = plot_title,
         x = "Peptide ID",
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