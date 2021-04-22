#' generate_butterfly_dataset
#' 
#' @param dat data as imported by the \code{\link{read_hdx}} function
#' @param protein ...
#' @param state ...
#' @param time_0 ...
#' @param time_100 ...
#' @param deut_part ...
#' 
#' @details ... 
#' 
#' @return ...
#' 
#' @seealso ...
#' 
#' @export generate_butterfly_dataset

generate_butterfly_dataset <- function(dat, 
                                       protein = unique(dat[["Protein"]])[1],
                                       state = (dat[["State"]])[1], 
                                       time_0 = 0.001,
                                       time_100 = 1440,
                                       deut_part = 1){
  
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

#' generate_butterfly_plot
#' 
#' @description Generates butterfly plot based on supplied data
#' and parameters.
#' 
#' @param butterfly_dat ... 
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param uncertainty_type ribbon / bars
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
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


#' generate_butterfly_data
#' 
#' @description Generates butterfly data, based on the supplied
#' parameters.
#' 
#' @param butterfly_dat data as imported by the \code{\link{read_hdx}} function
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' 
#' @details This data is available in the GUI. 
#' All of the numerical values are rounded to 4 places after the dot!!
#' 
#' @importFrom dplyr rename %>%
#' 
#' @return ...
#' 
#' @seealso ... 
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