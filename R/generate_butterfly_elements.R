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
#' @param uncertainty_type ...
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
                                    uncertainty_type = c("ribbon", "bars")){
  if (theoretical) {
    
    if (fractional) {
      # theoretical & fractional
      ggplot(butterfly_dat, aes(x = ID, y = theo_frac_deut_uptake, color = Exposure)) + 
        geom_point(aes(group = Exposure, color = Exposure)) + 
        geom_ribbon(aes(x = ID, ymin = theo_frac_deut_uptake - err_theo_frac_deut_uptake, ymax = theo_frac_deut_uptake + err_theo_frac_deut_uptake, fill = Exposure), alpha = 0.3, size = 0, linetype = "blank") + 
        coord_cartesian(ylim = c(0, NA)) +
        labs(x = "Peptide ID",
             y = "Theoretical fractional deuterium uptake [%]") +
        theme(legend.position = "bottom")
      
    } else {
      # theoretical & absolute
      ggplot(butterfly_dat, aes(x = ID, y = theo_deut_uptake, color = Exposure)) + 
        geom_point(aes(group = Exposure, color = Exposure)) + 
        geom_ribbon(aes(x = ID, ymin = theo_deut_uptake - err_theo_deut_uptake, ymax = theo_deut_uptake + err_theo_deut_uptake, fill = Exposure), alpha = 0.3, size = 0, linetype = "blank") + 
        coord_cartesian(ylim = c(0, NA)) +
        labs(x = "Peptide ID",
             y = "Theoretical deuterium uptake [Da]") +
        theme(legend.position = "bottom")
    } 
    
  } else {
    
    if (fractional) {
      # experimental & fractional
      ggplot(butterfly_dat, aes(x = ID, y = frac_deut_uptake, color = Exposure)) + 
        geom_point(aes(group = Exposure, color = Exposure)) + 
        geom_ribbon(aes(x = ID, ymin = frac_deut_uptake - err_frac_deut_uptake, ymax = frac_deut_uptake + err_frac_deut_uptake, fill = Exposure), alpha = 0.3, size = 0, linetype = "blank") + 
        coord_cartesian(ylim = c(0, NA)) +
        labs(x = "Peptide ID",
             y = "Fractional deuterium uptake [%]") +
        theme(legend.position = "bottom")
      
    } else {
      # experimental & absolute 
      ggplot(butterfly_dat, aes(x = ID, y = deut_uptake, color = Exposure)) + 
        geom_point(aes(group = Exposure, color = Exposure)) + 
        geom_ribbon(aes(x = ID, ymin = deut_uptake - err_deut_uptake, ymax = deut_uptake + err_deut_uptake, fill = Exposure), alpha = 0.3, size = 0, linetype = "blank") + 
        coord_cartesian(ylim = c(0, NA)) +
        labs(x = "Peptide ID",
             y = "Deuterium uptake [Da]") +
        theme(legend.position = "bottom")
    }
    
  }
  
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