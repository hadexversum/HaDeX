#' generate_butterfly_differential_dataset
#' 
#' @param dat ...
#' @param protein ...
#' @param state_1 ...
#' @param state_2 ...
#' @param time_0 ...
#' @param time_100 ...
#' @param deut_part ...
#' 
#' @details 
#' 
#' @return 
#' 
#' @seealso 
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


#' generate_butterfly_differential_plot
#' 
#' @param butterfly_diff_dat ...
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param uncertainty_type ribbon / bars
#' 
#' @details This plot is visible in GUI.
#' 
#' @return 
#' 
#' @seealso 
#' 
#' @export generate_butterfly_differential_plot

generate_butterfly_differential_plot <- function(butterfly_diff_dat, 
                                                 theoretical = FALSE, 
                                                 fractional = FALSE,
                                                 uncertainty_type = c("ribbon", "bars")){
  
  uncertainty_type <- match.arg(uncertainty_type, c("ribbon", "bars"))
  
  if(uncertainty_type == "ribbon"){
    
    if(theoretical){
      
      if(fractional){
        
        ggplot(butterfly_diff_dat, aes(x = ID, y = diff_theo_frac_deut_uptake, color = Exposure)) + 
          geom_point(aes(group = Exposure, color = Exposure)) + 
          geom_ribbon(aes(x = ID, ymin = diff_theo_frac_deut_uptake - err_diff_theo_frac_deut_uptake, ymax = diff_theo_frac_deut_uptake + err_diff_theo_frac_deut_uptake, fill = Exposure), alpha = 0.3, size = 0, linetype = "blank") + 
          coord_cartesian(ylim = c(-.5, 1)) +
          labs(title = "Theoretical butterfly differential plot",
               x = "Peptide ID",
               y = "Delta fractional deuterium uptake [%]") +
          theme(legend.position = "bottom")
        
      } else {
        
        ggplot(butterfly_diff_dat, aes(x = ID, y = diff_theo_deut_uptake, color = Exposure)) + 
          geom_point(aes(group = Exposure, color = Exposure)) + 
          geom_ribbon(aes(x = ID, ymin = diff_theo_deut_uptake - err_diff_theo_deut_uptake, ymax = diff_theo_deut_uptake + err_diff_theo_deut_uptake, fill = Exposure), alpha = 0.3, size = 0, linetype = "blank") + 
          coord_cartesian(ylim = c(-.5, 1)) +
          labs(title = "Theoretical butterfly differential plot",
               x = "Peptide ID",
               y = "Delta deuterium uptake [Da]") +
          theme(legend.position = "bottom")
      }
      
    } else {
      
      if(fractional){
        
        ggplot(butterfly_diff_dat, aes(x = ID, y = diff_frac_deut_uptake, color = Exposure)) + 
          geom_point(aes(group = Exposure, color = Exposure)) + 
          geom_ribbon(aes(x = ID, ymin = diff_frac_deut_uptake - err_diff_frac_deut_uptake, ymax = diff_frac_deut_uptake + err_diff_frac_deut_uptake, fill = Exposure), alpha = 0.3, size = 0, linetype = "blank") + 
          coord_cartesian(ylim = c(-.5, 1)) +
          labs(title = "Butterfly differential plot",
               x = "Peptide ID",
               y = "Delta fractional deuterium uptake [%]") +
          theme(legend.position = "bottom")
        
      } else {
        
        ggplot(butterfly_diff_dat, aes(x = ID, y = diff_deut_uptake, color = Exposure)) + 
          geom_point(aes(group = Exposure, color = Exposure)) + 
          geom_ribbon(aes(x = ID, ymin = diff_deut_uptake - err_diff_deut_uptake, ymax = diff_deut_uptake + err_diff_deut_uptake, fill = Exposure), alpha = 0.3, size = 0, linetype = "blank") + 
          coord_cartesian(ylim = c(-.5, 1)) +
          labs(title = "Butterfly differential plot",
               x = "Peptide ID",
               y = "Delta deuterium uptake [Da]") +
          theme(legend.position = "bottom")
        
      }
      
    }
    
  } else if (uncertainty_type == "bars"){
    
    if(theoretical){
      
      if(fractional){
        
        ggplot(butterfly_diff_dat, aes(x = ID, y = diff_theo_frac_deut_uptake, color = Exposure)) + 
          geom_point(aes(group = Exposure, color = Exposure)) + 
          geom_errorbar(aes(x = ID, ymin = diff_theo_frac_deut_uptake - err_diff_theo_frac_deut_uptake, ymax = diff_theo_frac_deut_uptake + err_diff_theo_frac_deut_uptake, color = Exposure), width = 0.25, alpha = 0.5) +
          coord_cartesian(ylim = c(-.5, 1)) +
          labs(title = "Theoretical butterfly differential plot",
               x = "Peptide ID",
               y = "Delta fractional deuterium uptake [%]") +
          theme(legend.position = "bottom")
        
      } else {
        
        ggplot(butterfly_diff_dat, aes(x = ID, y = diff_theo_deut_uptake, color = Exposure)) + 
          geom_point(aes(group = Exposure, color = Exposure)) + 
          geom_errorbar(aes(x = ID, ymin = diff_theo_deut_uptake - err_diff_theo_deut_uptake, ymax = diff_theo_deut_uptake + err_diff_theo_deut_uptake, color = Exposure), width = 0.25, alpha = 0.5) +
          coord_cartesian(ylim = c(-.5, 1)) +
          labs(title = "Theoretical butterfly differential plot",
               x = "Peptide ID",
               y = "Delta deuterium uptake [Da]") +
          theme(legend.position = "bottom")
      }
      
    } else {
      
      if(fractional){
        
        ggplot(butterfly_diff_dat, aes(x = ID, y = diff_frac_deut_uptake, color = Exposure)) + 
          geom_point(aes(group = Exposure, color = Exposure)) + 
          geom_errorbar(aes(x = ID, ymin = diff_frac_deut_uptake - err_diff_frac_deut_uptake, ymax = diff_frac_deut_uptake + err_diff_frac_deut_uptake, color = Exposure), width = 0.25, alpha = 0.5) +
          coord_cartesian(ylim = c(-.5, 1)) +
          labs(title = "Butterfly differential plot",
               x = "Peptide ID",
               y = "Delta fractional deuterium uptake [%]") +
          theme(legend.position = "bottom")
        
      } else {
        
        ggplot(butterfly_diff_dat, aes(x = ID, y = diff_deut_uptake, color = Exposure)) + 
          geom_point(aes(group = Exposure, color = Exposure)) + 
          geom_errorbar(aes(x = ID, ymin = diff_deut_uptake - err_diff_deut_uptake, ymax = diff_deut_uptake + err_diff_deut_uptake, color = Exposure), width = 0.25, alpha = 0.5) +
          coord_cartesian(ylim = c(-.5, 1)) +
          labs(title = "Butterfly differential plot",
               x = "Peptide ID",
               y = "Delta deuterium uptake [Da]") +
          theme(legend.position = "bottom")
        
      }
      
    }
  }
  
  # if (theoretical) {
  # 
  #   if (fractional) {
  # 
  #     # theoretical & fractional
  #     value <- "diff_theo_frac_deut_uptake"
  #     err_value <- "err_diff_theo_frac_deut_uptake"
  #     y_label <- "Fractional deuterium uptake difference [%]"
  #     title <- "Theoretical butterfly differential plot"
  # 
  #   } else {
  # 
  #     # theoretical & absolute
  #     value <- "diff_theo_deut_uptake"
  #     err_value <- "err_diff_theo_deut_uptake"
  #     y_label <- "Deuterium uptake difference [Da]"
  #     title <- "Theoretical butterfly differential plot"
  # 
  #   }
  # 
  # } else {
  # 
  #   if (fractional) {
  # 
  #     # experimental & fractional
  #     value <- "diff_frac_deut_uptake"
  #     err_value <- "err_diff_frac_deut_uptake"
  #     y_label <- "Fractional deuterium uptake difference [%]"
  #     title <- "Butterfly differential plot"
  # 
  #   } else {
  # 
  #     # experimental & absolute
  #     value <- "diff_deut_uptake"
  #     err_value <- "err_diff_deut_uptake"
  #     y_label <- "Deuterium uptake difference [Da]"
  #     title <- "Butterfly differential plot"
  # 
  #   }
  # 
  # }
  # 
  # butterfly_differential_plot <- ggplot(butterfly_diff_dat, aes(x = ID, y = get(value), color = Exposure)) +
  #     geom_point(aes(group = Exposure, color = Exposure)) +
  #     coord_cartesian(ylim = c(-.5, 1)) +
  #     labs(title = title,
  #          x = "Peptide ID",
  #          y = y_label) +
  #     theme(legend.position = "bottom")
  # 
  # if(uncertainty_type == "ribbon"){
  # 
  #   butterfly_differential_plot <- butterfly_differential_plot +
  #     geom_ribbon(aes(x = ID, ymin = get(value) - get(err_value), ymax = get(value) + get(err_value), fill = Exposure), alpha = 0.5, size = 0, linetype = "blank")
  # 
  # } else if (uncertainty_type == "bars"){
  # 
  #   butterfly_differential_plot <- butterfly_differential_plot +
  #     geom_errorbar(aes(x = ID, ymin = get(value) - get(err_value), ymax = get(value) + get(err_value), color = Exposure), width = 0.25, alpha = 0.5)
  # 
  # }
  # 
  # return(butterfly_differential_plot)
    
}

#' generate_butterfly_differential_data
#' 
#' @param butterfly_diff_dat ...
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' 
#' @details 
#' 
#' @return 
#' 
#' @seealso 
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