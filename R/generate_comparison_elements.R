#' generate_comparison_dataset
#' 
#' @description Generates comparison data set for given states
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. Default value - first protein name from 
#' the data.
#' @param states vector of states (for chosen protein), for which the 
#' calculations are done. Default value - vector of all states from the
#' data.
#' @param time_0 minimal exchange control. Default value - 0.001.
#' @param time_t time point of the measurement for which the calculations
#' are done. Default value - 1.
#' @param time_100 maximal exchange control. Default value - 1440.
#' @param deut_part deuterium percentage in solution used in experiment.
#' Default value - 1.
#' 
#' @details This is internal function, not used in the application.
#' This function is a wrapper for \code{\link{calculate_state_deuteration}}
#' function, calls this function for all states in states vector.
#' 
#' @return a data frame with calculated deuterium uptake, fractional deuterium 
#' uptake, theoretical deuterium uptake, theoretical fractional deuterium uptake
#' and their uncertainty, based on supplied parameters. 
#'
#' @seealso \code{\link{calculate_state_deuteration}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' generate_comparison_dataset(dat)
#'
#' @export generate_comparison_dataset

generate_comparison_dataset <- function(dat,
                                        protein = unique(dat[["Protein"]])[1],
                                        states = unique(dat[["State"]]),
                                        time_0 = 0.001,
                                        time_t = 1,
                                        time_100 = 1440,
                                        deut_part = 1){
  
  
  lapply(states, function(state){
    
    calculate_state_deuteration(dat,
                                protein = protein,
                                state = state,
                                time_0 = time_0,
                                time_t = time_t,
                                time_100 = time_100,
                                deut_part = deut_part)
    
  }) %>% bind_rows
  
}

#' generate_comparison_plot
#' 
#' @description Generates comparison plot based on supplied data
#' and parameters.
#' 
#' @param dat produced by \code{\link{calculate_state_deuteration}} or
#' \code{\link{generate_comparison_dataset}} function
#' @param theoretical \code{logical}, determines if values are theoretical. 
#' Default value - FALSE.
#' @param fractional \code{logical}, determines if values are fractional.
#' Default value - FALSE.
#' 
#' @importFrom ggplot2 ggplot geom_segment geom_errorbar theme scale_y_continuous
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return a \code{ggplot} object.
#' 
#' @seealso \code{\link{calculate_state_deuteration}}  \code{\link{generate_comparison_dataset}}
#' \code{\link{generate_comparison_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' comparison_dat <- generate_comparison_dataset(dat)
#' generate_comparison_plot(comparison_dat)
#' 
#' @export generate_comparison_plot

generate_comparison_plot <- function(dat, 
                                     theoretical = FALSE, 
                                     fractional = FALSE){
  
  if (theoretical) {
    
    title <- "Theoretical comparison plot"
    
    if (fractional) {
      
      # theoretical & fractional
      value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      y_label <- "fractional deuterium uptake [%]"
      
    } else {
      
      # theoretical & absolute
      value <- "theo_deut_uptake"
      err_value <- "err_theo_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      
    } 
    
  } else {
    
    title <- "Comparison plot"
    
    if (fractional) {
      
      # experimantal & fractional
      value <- "frac_deut_uptake"
      err_value <- "err_frac_deut_uptake"
      y_label <- "fractional deuterium uptake [%]"
      
    } else {
      
      # experimental & absolute 
      value <- "deut_uptake"
      err_value <- "err_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      
    }
    
  }
  
  plot_dat <- data.frame(Sequence = dat[["Sequence"]],
                         Start = dat[["Start"]],
                         End = dat[["End"]],
                         Med_Sequence = dat[["Med_Sequence"]],
                         State = dat[["State"]],
                         value = dat[[value]],
                         err_value = dat[[err_value]])
  
  ggplot(data = plot_dat) +
    geom_segment(data = plot_dat, aes(x = Start, y = value, xend = End, yend = value, color = State)) +
    geom_errorbar(data = plot_dat, aes(x = Med_Sequence, ymin = value - err_value, ymax = value + err_value, color = State)) +
    labs(title = title,
         x = "Position in the sequence",
         y = y_label) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
}


#' generate_comparison_data
#' 
#' @description Generates comparison data, based on the supplied
#' parameters.
#' 
#' @importFrom dplyr rename
#'  
#' @param dat produced by \code{\link{calculate_state_deuteration}} or
#' \code{\link{generate_comparison_dataset}} function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' Default value - FALSE.
#' @param fractional \code{logical}, determines if values are fractional
#' Default value - FALSE.
#' 
#' @details This function rounds the numerical values (4 places) and 
#' changes the column names to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @return a \code{data.frame} object.
#' 
#' @seealso \code{\link{calculate_state_deuteration}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' comparison_dat <- generate_comparison_dataset(dat)
#' generate_comparison_data(comparison_dat)
#' 
#' @export generate_comparison_data

generate_comparison_data <- function(dat, 
                                     theoretical = FALSE, 
                                     fractional = FALSE,
                                     protein){
  if (theoretical){
    
    if (fractional){
      
      # theoretical & fractional
      dat %>%
        select(Protein, Sequence, State, Start, End, theo_frac_deut_uptake, err_theo_frac_deut_uptake) %>%
        mutate(theo_frac_deut_uptake  = round(theo_frac_deut_uptake , 4),
               err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Frac Exch" = theo_frac_deut_uptake , 
               "Err Theo Frac Exch" = err_theo_frac_deut_uptake)
      
    } else {
      
      # theoretical & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, theo_deut_uptake, err_theo_deut_uptake) %>%
        mutate(theo_deut_uptake = round(theo_deut_uptake, 4),
               err_theo_deut_uptake = round(err_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Abs Val Exch" = theo_deut_uptake,
               "Err Theo Abs Val Exch" = err_theo_deut_uptake)
    }
    
  } else {
    
    if (fractional){
      
      # experimental & fractional
      dat %>%
        select(Protein, Sequence, State, Start, End, frac_deut_uptake, err_frac_deut_uptake) %>%
        mutate(frac_deut_uptake = round(frac_deut_uptake, 4),
               err_frac_deut_uptake = round(err_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Frac Exch" = frac_deut_uptake,
               "Err Frac Exch" = err_frac_deut_uptake)
      
    } else {
      
      # experimental & absolute
      dat %>%
        select(Protein, Sequence, State, Start, End, deut_uptake, err_deut_uptake) %>%
        mutate(deut_uptake = round(deut_uptake, 4),
               err_deut_uptake = round(err_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Abs Val Exch" = deut_uptake,
               "Err Abs Val Exch" = err_deut_uptake)
      
    }
    
  }
  
}