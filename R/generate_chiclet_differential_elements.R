#' generate_chiclet_differential_plot
#' 
#' @param chiclet_diff_dat ...
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param show_uncertainty ...
#' 
#' @details This plot is visible in GUI.
#' 
#' @return 
#' 
#' @seealso 
#' 
#' @export generate_chiclet_differential_plot

generate_chiclet_differential_plot <- function(chiclet_diff_dat, 
                                               theoretical = FALSE, 
                                               fractional = FALSE,
                                               show_uncertainty = "ribbon"){
  
 
  if (theoretical) {
    
    if (fractional) {
      
      # theoretical & fractional
      value <- "diff_theo_frac_deut_uptake"
      err_value <- "err_diff_theo_frac_deut_uptake"
      title <- "Theoretical chiclet differential plot"
      fill <- "Fractional DU Diff"
      
    } else {
      
      # theoretical & absolute
      value <- "diff_theo_deut_uptake"
      err_value <- "err_diff_theo_deut_uptake"
      title <- "Theoretical chiclet differential plot"
      fill <- "DU Diff"
      
    }
    
  } else {
    
    if (fractional) {
      
      # experimental & fractional
      value <- "diff_frac_deut_uptake"
      err_value <- "err_diff_frac_deut_uptake"
      title <- "Chiclet differential plot"
      fill <- "Fractional DU Diff"
      
    } else {
      
      # experimental & absolute
      value <- "diff_deut_uptake"
      err_value <- "err_diff_deut_uptake"
      title <- "Chiclet differential plot"
      fill <- "DU Diff"
      
    }
    
  }
  
  plot_dat <- data.frame(ID = chiclet_diff_dat[["ID"]],
                         Exposure = chiclet_diff_dat[["Exposure"]],
                         value = chiclet_diff_dat[[value]],
                         err_value = chiclet_diff_dat[[err_value]],
                         Sequence = chiclet_diff_dat[["Sequence"]],
                         Start = chiclet_diff_dat[["Start"]],
                         End = chiclet_diff_dat[["End"]])
  
  chiclet_differential_plot <- ggplot(plot_dat, aes(y = as.factor(Exposure), x = ID)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", guide = guide_legend(keywidth = 3)) +
    labs(title = title,
         y = "Exposure [min]",
         x = "Peptide ID",
         fill = fill) +
    theme(legend.position = "bottom",
          legend.box = "vertical")
  
  if(show_uncertainty){
    
    chiclet_differential_plot <- chiclet_differential_plot +
      geom_point(aes(size = err_value), shape = 3) + 
      labs(size = "Err")
    
  } 
  
  return(chiclet_differential_plot)
  
}

#' generate_chiclet_differential_data
#' 
#' @param chiclet_diff_dat ...
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' 
#' @details 
#' 
#' @return 
#' 
#' @seealso 
#' 
#' @export generate_chiclet_differential_data

generate_chiclet_differential_data <- function(chiclet_diff_dat, 
                                               theoretical = FALSE, 
                                               fractional = FALSE){
  
  if(theoretical){
    
    if(fractional){
      
      chiclet_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake) %>%
        mutate(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
               err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Theo Frac DU" = diff_theo_frac_deut_uptake,
               "Err Diff Theo Frac DU" = err_diff_theo_frac_deut_uptake)
      
    } else {
      
      chiclet_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_theo_deut_uptake, err_diff_theo_deut_uptake) %>%
        mutate(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
               err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Theo DU" = diff_theo_deut_uptake,
               "Err Diff Theo DU" = err_diff_theo_deut_uptake)
      
    }
    
  } else {
    
    if(fractional){
      
      chiclet_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_frac_deut_uptake, err_diff_frac_deut_uptake) %>%
        mutate(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
               err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Frac DU" = diff_frac_deut_uptake,
               "Err Diff Frac DU" = err_diff_frac_deut_uptake)
      
    } else {
      
      chiclet_diff_dat %>%
        select(Protein, Sequence, ID, Start, End, Exposure, diff_deut_uptake, err_diff_deut_uptake) %>%
        mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
               err_diff_deut_uptake = round(err_diff_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff DU" = diff_deut_uptake,
               "Err Diff DU" = err_diff_deut_uptake)
      
    }
    
  }
  
}