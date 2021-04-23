#' generate_chiclet_plot
#' 
#' @description Generates chiclet plot based on supplied data
#' and parameters.
#' 
#' @param chiclet_dat produced by generate_butterfly_dataset 
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param show_uncertainty ...
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_chiclet_plot

generate_chiclet_plot <- function(chiclet_dat, 
                                  theoretical = FALSE, 
                                  fractional = FALSE,
                                  show_uncertainty = FALSE){

  state <- unique(chiclet_dat[["State"]])
  
  if (theoretical) {
    
    if (fractional) {
      
      # theoretical & fractional
      value <- "theo_frac_deut_uptake"
      err_value <- "err_theo_frac_deut_uptake"
      title <- paste0("Theoretical chiclet plot for ", state, " state")
      fill <- "Fractional DU"
      
    } else {
      
      # theoretical & absolute
      value <- "theo_deut_uptake"
      err_value <- "err_theo_deut_uptake"
      title <- paste0("Theoretical chiclet plot for ", state, " state")
      fill <- "DU"
    }
    
  } else {
    
    if (fractional) {
      
      # experimental & fractional
      value <- "frac_deut_uptake"
      err_value <- "err_frac_deut_uptake"
      title <- paste0("Chiclet plot for ", state, " state")
      fill <- "Fractional DU"
      
    } else {
      
      # experimental & absolute
      value <- "deut_uptake"
      err_value <- "err_deut_uptake"
      title <- paste0("Chiclet plot for ", state, " state")
      fill <- "DU"
      
    }
    
  }
  
  plot_dat <- data.frame(ID = chiclet_dat[["ID"]],
                         Exposure = chiclet_dat[["Exposure"]],
                         value = chiclet_dat[[value]],
                         err_value = chiclet_dat[[err_value]],
                         Sequence = chiclet_dat[["Sequence"]],
                         Start = chiclet_dat[["Start"]],
                         End = chiclet_dat[["End"]])
  
  chiclet_plot <- ggplot(plot_dat, aes(y = Exposure, x = ID)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", guide = guide_legend(keywidth = 3)) +
    labs(title = title,
         y = "Exposure [min]",
         x = "Peptide ID",
         fill = fill) +
    theme(legend.position = "bottom",
          legend.box = "vertical")
  
  if(show_uncertainty){
    
    chiclet_plot <- chiclet_plot +
      geom_point(aes(size = err_value), shape = 3) + 
      labs(size = "Err")
    
  } 
  
  return(chiclet_plot)
  
}

#' generate_chiclet_data
#' 
#' @description Generates chiclet data, based on the supplied
#' parameters.
#' 
#' @param chiclet_dat ...
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
#' @export generate_chiclet_data

generate_chiclet_data <- function(chiclet_dat, 
                                  theoretical = FALSE, 
                                  fractional = FALSE){
  
  
  if (theoretical){
    
    if (fractional){
      # theoretical & fractional
      chiclet_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, theo_frac_deut_uptake, err_theo_frac_deut_uptake) %>%
        mutate(theo_frac_deut_uptake  = round(theo_frac_deut_uptake , 4),
               err_theo_frac_deut_uptake = round(err_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Frac Exch" = theo_frac_deut_uptake , 
               "Err Theo Frac Exch" = err_theo_frac_deut_uptake)
      
    } else {
      # theoretical & absolute
      chiclet_dat %>%
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
      chiclet_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, frac_deut_uptake, err_frac_deut_uptake) %>%
        mutate(frac_deut_uptake = round(frac_deut_uptake, 4),
               err_frac_deut_uptake = round(err_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Frac Exch" = frac_deut_uptake,
               "Err Frac Exch" = err_frac_deut_uptake)
      
    } else {
      # experimental & absolute
      chiclet_dat %>%
        select(Protein, Sequence, ID, State, Start, End, Exposure, deut_uptake, err_deut_uptake) %>%
        mutate(deut_uptake = round(deut_uptake, 4),
               err_deut_uptake = round(err_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Abs Val Exch" = deut_uptake,
               "Err Abs Val Exch" = err_deut_uptake)
      
    }
    
  }
  
}
