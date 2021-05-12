#' Generates butterfly dataset
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state biological state for chosen protein.
#' @param time_0 minimal exchange control time point of measurement.
#' @param time_100 maximal exchange control time point of measurement. 
#' @param deut_part deuterium percentage in solution used in experiment, 
#' value from range [0, 1].
#' 
#' @details The function \code{\link{generate_butterfly_dataset}} generates 
#' a dataset what can be plotted in a form of a butterfly plot. For each
#' peptide in chosen protein in chosen state for time points of measurement
#' between minimal and maximal control time points of measurement deuterium 
#' uptake, fractional deuterium uptake with respect to controls or theoretical
#' tabular values are calculated, with combined and propagated uncertainty. 
#' Each peptide has an ID, based on its start position.
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_state_deuteration}}
#' \code{\link{generate_butterfly_plot}} 
#' \code{\link{generate_butterfly_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' butterfly_dat <- generate_butterfly_dataset(dat)
#' head(butterfly_dat)
#' 
#' @export generate_butterfly_dataset

generate_butterfly_dataset <- function(dat, 
                                       protein = unique(dat[["Protein"]])[1],
                                       state = (dat[["State"]])[1], 
                                       time_0 = 0.001,
                                       time_100 = 1440,
                                       deut_part = 0.9){
  
  all_times <- unique(dat[["Exposure"]])
  times <- all_times[all_times > time_0 & all_times < time_100]
  
  butterfly_dat <- lapply(times, function(t){
    
    calculate_state_deuteration(dat, protein = protein, state = state,
                                time_0 = time_0, time_t = t, time_100 = time_100, deut_part = deut_part) %>%
      arrange(Start, End) %>%
      mutate(ID = 1L:nrow(.),
             Exposure = factor(t)) %>%
      select(ID, Exposure, everything()) 
    
  }) %>% bind_rows()
  
  return(butterfly_dat)
  
}


#' Generate butterfly plot
#' 
#' @param butterfly_dat data produced by \code{\link{generate_butterfly_dataset}} 
#' function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' @param uncertainty_type type of presenting uncertainty, possible values:
#' "ribbon", "bars" or "bars + line".
#' 
#' @details Function \code{\link{generate_butterfly_plot}} generates butterfly plot
#' based on provided data and parameters. On X-axis there is peptide ID. On the Y-axis
#' there is deuterium uptake in chosen form. Data from multiple time points of 
#' measurement is presented.
#' This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot}} object.
#' 
#' @seealso 
#' \code{\link{generate_butterfly_dataset}} 
#' \code{\link{generate_butterfly_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' butterfly_dat <- generate_butterfly_dataset(dat)
#' generate_butterfly_plot(butterfly_dat)
#' 
#' @export generate_butterfly_plot

generate_butterfly_plot <- function(butterfly_dat, 
                                    theoretical = FALSE, 
                                    fractional = FALSE,
                                    uncertainty_type = "ribbon"){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars", "bars + line"))
  state <- unique(butterfly_dat[["State"]])
  
  if (theoretical) {

    if (fractional) {

      # theoretical & fractional
      value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake [%]"
      title <- paste0("Theoretical butterfly plot for ", state, " state")

    } else {

      # theoretical & absolute
      value <- "theo_deut_uptake"
      err_value <- "err_theo_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      title <- paste0("Theoretical butterfly plot for ", state, " state")

    }

  } else {

    if (fractional) {

      # experimental & fractional
      value <- "frac_deut_uptake"
      err_value <- "err_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake [%]"
      title <- paste0("Butterfly plot for ", state, " state")

    } else {

      # experimental & absolute
      value <- "deut_uptake"
      err_value <- "err_deut_uptake"
      y_label <- "Deuterium uptake [Da]"
      title <- paste0("Butterfly plot for ", state, " state")

    }

  }
  
  plot_dat <- data.frame(ID = butterfly_dat[["ID"]],
                         Exposure = butterfly_dat[["Exposure"]],
                         value = butterfly_dat[[value]],
                         err_value = butterfly_dat[[err_value]],
                         Sequence = butterfly_dat[["Sequence"]],
                         Start = butterfly_dat[["Start"]],
                         End = butterfly_dat[["End"]])
  
  butterfly_plot <- ggplot(plot_dat, aes(x = ID, y = value, color = Exposure)) +
    geom_point(aes(group = Exposure, color = Exposure)) +
    coord_cartesian(ylim = c(0, NA)) +
    labs(x = "Peptide ID",
         y = y_label) +
    theme(legend.position = "bottom")

  if(uncertainty_type == "ribbon"){

    butterfly_plot <- butterfly_plot +
    geom_ribbon(aes(x = ID, ymin = value - err_value, ymax = value + err_value, fill = Exposure), alpha = 0.5, size = 0, linetype = "blank")

  } else if (uncertainty_type == "bars") {

    butterfly_plot <- butterfly_plot +
      geom_errorbar(aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5)

  } else if (uncertainty_type == "bars + line"){
    
    butterfly_plot <- butterfly_plot +
      geom_errorbar(aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5) +
      geom_line()
    
  }
  
  return(butterfly_plot)
  
}


#' Generate butterfly data
#' 
#' @param butterfly_dat data produced by \code{\link{generate_butterfly_dataset}} 
#' function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' 
#' @details This function subsets the dataset based on provided criteria,
#' rounds the numerical values (4 places) and changes the column names 
#' to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @importFrom dplyr rename %>%
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{generate_butterfly_dataset}} 
#' \code{\link{generate_butterfly_plot}} 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' butterfly_dat <- generate_butterfly_dataset(dat)
#' generate_butterfly_data(butterfly_dat)
#' 
#' @export generate_butterfly_data

generate_butterfly_data <- function(butterfly_dat, 
                                    theoretical = FALSE, 
                                    fractional = FALSE){
  
  
  if (theoretical){
    
    if (fractional){
      
      # theoretical & fractional
      butterfly_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, theo_frac_deut_uptake, err_theo_frac_deut_uptake) %>%
        mutate(theo_frac_deut_uptake  = round(theo_frac_deut_uptake , 4),
               err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Frac Exch" = theo_frac_deut_uptake , 
               "Err Theo Frac Exch" = err_theo_frac_deut_uptake)
      
    } else {
      
      # theoretical & absolute
      butterfly_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, theo_deut_uptake, err_theo_deut_uptake) %>%
        mutate(theo_deut_uptake = round(theo_deut_uptake, 4),
               err_theo_deut_uptake = round(err_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Abs Val Exch" = theo_deut_uptake,
               "Err Theo Abs Val Exch" = err_theo_deut_uptake)
    }
    
  } else {
    
    if (fractional){
      
      # experimental & fractional
      butterfly_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, frac_deut_uptake, err_frac_deut_uptake) %>%
        mutate(frac_deut_uptake = round(frac_deut_uptake, 4),
               err_frac_deut_uptake = round(err_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Frac Exch" = frac_deut_uptake,
               "Err Frac Exch" = err_frac_deut_uptake)
      
    } else {
      
      # experimental & absolute
      butterfly_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, deut_uptake, err_deut_uptake) %>%
        mutate(deut_uptake = round(deut_uptake, 4),
               err_deut_uptake = round(err_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Abs Val Exch" = deut_uptake,
               "Err Abs Val Exch" = err_deut_uptake)
      
    }
    
  }
  
}