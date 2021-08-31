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



#' 
#' 
#' @description ...
#' 
#' @param replicate_masses ...
#' @param time_t ...
#' @param protein ...
#' @param state ...
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

show_replicate_data <- function(replicate_masses,
                                time_t,
                                protein, 
                                state){
  
  replicate_masses %>%
    filter(Exposure == time_t,
           Protein == protein,
           State == state) %>%
    select(Sequence, Start, End, ID) %>%
    group_by(Sequence, Start, End, ID) %>%
    summarize(n = n())
  
}

#' 
#' 
#' @description ...
#' 
#' @param replicates_histogram_data ...
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

plot_replicate_data <- function(replicates_histogram_data){
  
  replicates_histogram_data %>%
    ggplot() + 
    geom_col(aes(x = ID, y = n, fill = n)) +
    labs(title = paste0("Number of replicates for each peptide"),
         x = "Peptide ID",
         y = "Number of replicates") +
    theme(legend.position = "none")
  
}

#' 
#' 
#' @description ...
#' 
#' @param replicates_histogram_data ...
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

show_all_replicate_data <- function(replicates_histogram_data){
  
  replicates_histogram_data %>%
    arrange(ID)
  
}

#' 
#' 
#' @description ...
#' 
#' @param replicate_masses ...
#' @param protein ...
#' @param state ...
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

plot_all_replicate_data <- function(replicate_masses,
                                    protein, 
                                    state){
  
  replicate_masses %>%
    filter(Protein == protein,
           State == state,
           Exposure < 99999) %>%
    select(Sequence, Exposure, Start, End, ID) %>%
    group_by(Sequence, Exposure, Start, End, ID) %>%
    summarize(n = n())
  
}