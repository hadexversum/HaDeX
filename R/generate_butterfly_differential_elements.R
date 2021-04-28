#' Generate butterfly differential dataset
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
#' @details The function \code{\link{generate_butterfly_differential_dataset}} 
#' generates a dataset what can be plotted in a form of a butterfly differential 
#' plot. For each peptide in chosen protein for time points of measurement
#' between minimal and maximal control time points of measurement deuterium 
#' uptake difference, fractional deuterium uptake difference with respect to 
#' controls or theoretical tabular values are calculated, with combined and 
#' propagated uncertainty. Each peptide has an ID, based on its start
#' position.
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{generate_differential_data_set}}
#' \code{\link{generate_butterfly_differential_plot}}
#' \code{\link{generate_butterfly_differential_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' butterfly_diff_dat <- generate_butterfly_differential_dataset(dat)
#' head(butterfly_diff_dat)
#' 
#' @export generate_butterfly_differential_dataset

generate_butterfly_differential_dataset <- function(dat, 
                                                    protein = unique(dat[["Protein"]])[1],
                                                    state_1 = unique(dat[["State"]])[1],
                                                    state_2 = unique(dat[["State"]])[2], 
                                                    time_0 = 0.001,
                                                    time_100 = 1440,
                                                    deut_part = 1){
  
  all_times <- unique(dat[["Exposure"]])
  times <- all_times[all_times > time_0 & all_times < time_100]
  
  butterfly_diff_dat <- lapply(times, function(t){
    
    generate_differential_data_set(dat = dat, states = c(state_1, state_2), protein = protein, 
                                   time_0 = time_0, time_t = t, time_100 = time_100, deut_part = deut_part) %>%
      arrange(Start, End) %>%
      mutate(ID = 1L:nrow(.),
             Exposure = factor(t)) %>%
      select(ID, Exposure, everything()) 
    
  }) %>% bind_rows() %>%
    ungroup(.)
  
  return(butterfly_diff_dat)
  
}


#' Generate butterfly differential plot
#' 
#' @param butterfly_diff_dat data produced by 
#' \code{\link{generate_butterfly_differential_dataset}} function.
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param uncertainty_type type of presenting uncertainty, possible values: 
#' "ribbon", "bars" or "bars + line".
#' 
#' @details Function \code{\link{generate_butterfly_differential_plot}} generates 
#' butterfly differential plot based on provided data and parameters. On X-axis 
#' there is peptide ID. On the Y-axis there is deuterium uptake difference in 
#' chosen form. Data from multiple time points of measurement is presented.
#' This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot}} object. 
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{generate_differential_data_set}}
#' \code{\link{generate_butterfly_differential_dataset}}
#' \code{\link{generate_butterfly_differential_data}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' butterfly_diff_dat <- generate_butterfly_differential_dataset(dat)
#' generate_butterfly_differential_plot(butterfly_diff_dat)
#' 
#' @export generate_butterfly_differential_plot

generate_butterfly_differential_plot <- function(butterfly_diff_dat, 
                                                 theoretical = FALSE, 
                                                 fractional = FALSE,
                                                 uncertainty_type = "ribbon"){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars", "bars + line"))
  


  if (theoretical) {

    if (fractional) {

      # theoretical & fractional
      value <- "diff_theo_frac_deut_uptake"
      err_value <- "err_diff_theo_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake difference [%]"
      title <- "Theoretical butterfly differential plot"

    } else {

      # theoretical & absolute
      value <- "diff_theo_deut_uptake"
      err_value <- "err_diff_theo_deut_uptake"
      y_label <- "Deuterium uptake difference [Da]"
      title <- "Theoretical butterfly differential plot"

    }

  } else {

    if (fractional) {

      # experimental & fractional
      value <- "diff_frac_deut_uptake"
      err_value <- "err_diff_frac_deut_uptake"
      y_label <- "Fractional deuterium uptake difference [%]"
      title <- "Butterfly differential plot"

    } else {

      # experimental & absolute
      value <- "diff_deut_uptake"
      err_value <- "err_diff_deut_uptake"
      y_label <- "Deuterium uptake difference [Da]"
      title <- "Butterfly differential plot"

    }

  }

  plot_dat <- data.frame(ID = butterfly_diff_dat[["ID"]],
                         Exposure = butterfly_diff_dat[["Exposure"]],
                         value = butterfly_diff_dat[[value]],
                         err_value = butterfly_diff_dat[[err_value]],
                         Sequence = butterfly_diff_dat[["Sequence"]],
                         Start = butterfly_diff_dat[["Start"]],
                         End = butterfly_diff_dat[["End"]])
  
  butterfly_differential_plot <- ggplot(plot_dat, aes(x = ID, y = value, color = Exposure)) +
      geom_point(aes(group = Exposure, color = Exposure)) +
      coord_cartesian(ylim = c(-.5, 1)) +
      labs(title = title,
           x = "Peptide ID",
           y = y_label) +
      theme(legend.position = "bottom")

  if(uncertainty_type == "ribbon"){

    butterfly_differential_plot <- butterfly_differential_plot +
      geom_ribbon(aes(x = ID, ymin = value - err_value, ymax = value + err_value, fill = Exposure), alpha = 0.5, size = 0, linetype = "blank")

  } else if (uncertainty_type == "bars"){

    butterfly_differential_plot <- butterfly_differential_plot +
      geom_errorbar(aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5)

  } else if (uncertainty_type == "bars + line"){
    
    butterfly_differential_plot <- butterfly_differential_plot +
      geom_errorbar(aes(x = ID, ymin = value - err_value, ymax = value + err_value, color = Exposure), width = 0.25, alpha = 0.5) +
      geom_line()
    
  }

  return(butterfly_differential_plot)
    
}

#' Generate butterfly differential data
#' 
#' @param butterfly_diff_dat data produced by 
#' \code{\link{generate_butterfly_differential_dataset}} function.
#' @param theoretical \code{logical}, determines if values are theoretical.
#' @param fractional \code{logical}, determines if values are fractional.
#' 
#' @details This function subsets the dataset based on provided criteria,
#' rounds the numerical values (4 places) and changes the column names 
#' to user-friendly ones. 
#' This data is available in the GUI. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{generate_differential_data_set}}
#' \code{\link{generate_butterfly_differential_plot}}
#' \code{\link{generate_butterfly_differential_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' butterfly_diff_dat <- generate_butterfly_differential_dataset(dat)
#' head(generate_butterfly_differential_data(butterfly_diff_dat))
#' 
#' @export generate_butterfly_differential_data

generate_butterfly_differential_data <- function(butterfly_diff_dat, 
                                                 theoretical = FALSE, 
                                                 fractional = FALSE){
  
  if(theoretical){
    
    if(fractional){
      
      butterfly_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake) %>%
        mutate(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
               err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Theo Frac DU" = diff_theo_frac_deut_uptake,
               "Err Diff Theo Frac DU" = err_diff_theo_frac_deut_uptake)
      
    } else {
      
      butterfly_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_deut_uptake, err_diff_theo_deut_uptake) %>%
        mutate(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
               err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Theo DU" = diff_theo_deut_uptake,
               "Err Diff Theo DU" = err_diff_theo_deut_uptake)
      
    }
    
  } else {
    
    if(fractional){
      
      butterfly_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_frac_deut_uptake, err_diff_frac_deut_uptake) %>%
        mutate(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
               err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Frac DU" = diff_frac_deut_uptake,
               "Err Diff Frac DU" = err_diff_frac_deut_uptake)
      
    } else {
      
      butterfly_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_deut_uptake, err_diff_deut_uptake) %>%
        mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
               err_diff_deut_uptake = round(err_diff_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff DU" = diff_deut_uptake,
               "Err Diff DU" = err_diff_deut_uptake)
      
    }
    
  }
  
}