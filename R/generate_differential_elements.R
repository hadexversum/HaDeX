#' generate_differential_data_set
#' 
#' @description Generate the data frame with differential data from two
#' provided states - experimental/fractional calculations with the
#' uncertainty, based on supplied parameters.
#' 
#' @importFrom tidyr gather
#' 
#' @param dat ...
#' @param states vector of two states to calculate difference between them, 
#' the order is important (state first - state second)
#' @param protein ...
#' @param time_0 ...
#' @param time_t ...
#' @param time_100 ...
#' @param deut_part ...
#' 
#' @details The names of the parameters and variables will be changed 
#' later after the glossary project.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @examples
#' # load example data
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' # calculate differential values between states "CD160" and "CD160_HVEM" for protein "db_CD160"
#' generate_differential_data_set(dat = dat, states = c("CD160", "CD160_HVEM"), protein = "db_CD160", 
#'                                time_0 = 0.001, time_t = 5.000, time_100 = 1440.000)
#' 
#' @export generate_differential_data_set

generate_differential_data_set <- function(dat,
                                           states,
                                           protein,
                                           time_0,
                                           time_t,
                                           time_100,
                                           deut_part = 1){
  
  bind_rows(lapply(states, function(i) calculate_state_deuteration(dat, 
                                                                   protein = protein, 
                                                                   state = i, 
                                                                   time_0 = time_0,
                                                                   time_t = time_t, 
                                                                   time_100 = time_100,
                                                                   deut_part = deut_part))) %>%
    droplevels() %>% 
    mutate(State = factor(State, levels = states, labels = c("1", "2"))) %>%
    gather(variable, value, -c(Protein:End, State, Med_Sequence)) %>%
    unite(tmp, variable, State) %>%
    spread(tmp, value)  %>%
    mutate(diff_frac_deut_uptake = frac_deut_uptake_1 - frac_deut_uptake_2,
           err_diff_frac_deut_uptake = sqrt(err_frac_deut_uptake_1^2 + err_frac_deut_uptake_2^2),
           diff_deut_uptake = deut_uptake_1 - deut_uptake_2,
           err_diff_deut_uptake = sqrt(err_deut_uptake_1^2 + err_deut_uptake_2^2),
           diff_theo_frac_deut_uptake = theo_frac_deut_uptake_1 - theo_frac_deut_uptake_2, 
           err_diff_theo_frac_deut_uptake = sqrt(err_theo_frac_deut_uptake_1^2 + err_theo_frac_deut_uptake_2^2),
           diff_theo_deut_uptake = theo_deut_uptake_1 - theo_deut_uptake_2,
           err_diff_theo_deut_uptake = sqrt(err_theo_deut_uptake_1^2 + err_theo_deut_uptake_2^2)) %>%
    arrange(Start, End) %>%
    select(Protein, Start, End, Med_Sequence, everything(), -contains("1"), -contains("2"))
}


#' generate_differential_data
#' 
#' @description Generates differential data, based on the supplied
#' parameters.
#' 
#' @param dat custom format, produced by 
#' \code{\link{generate_differential_data_set}}
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param confidence_limit_1 ...
#' @param confidence_limit_2 ...
#' 
#' @details This data is available in the GUI. 
#' All of the numerical values are rounded to 4 places after the dot!!
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_differential_data

generate_differential_data <- function(dat, 
                                       theoretical, 
                                       fractional,
                                       confidence_limit_1,
                                       confidence_limit_2){
  
  column_name_cl1 <- paste0("Valid At ", confidence_limit_1)
  column_name_cl2 <- paste0("Valid At ", confidence_limit_2)
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional  
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = TRUE, 
                            fractional = TRUE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = TRUE, 
                            fractional = TRUE) %>%
        select(Protein, Sequence, Start, End, diff_theo_frac_deut_uptake, err_diff_theo_frac_deut_uptake, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_theo_frac_deut_uptake = round(diff_theo_frac_deut_uptake, 4),
               err_diff_theo_frac_deut_uptake = round(err_diff_theo_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Diff Frac Exch" = diff_theo_frac_deut_uptake,
               "Err Theo Diff Frac Exch" = err_diff_theo_frac_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
      
    } else {
      # theoretical & absolute
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = TRUE, 
                            fractional = FALSE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = TRUE, 
                            fractional = FALSE) %>%
        select(Protein, Sequence, Start, End, diff_theo_deut_uptake, err_diff_theo_deut_uptake, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_theo_deut_uptake = round(diff_theo_deut_uptake, 4),
               err_diff_theo_deut_uptake = round(err_diff_theo_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Theo Abs Value Diff" = diff_theo_deut_uptake,
               "Err Theo Abs Value Diff" = err_diff_theo_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = FALSE, 
                            fractional = TRUE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = FALSE, 
                            fractional = TRUE) %>%
        select(Protein, Sequence, Start, End, diff_frac_deut_uptake, err_diff_frac_deut_uptake, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_frac_deut_uptake = round(diff_frac_deut_uptake, 4),
               err_diff_frac_deut_uptake = round(err_diff_frac_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Frac Exch" = diff_frac_deut_uptake,
               "Err Diff Frac Exch" = err_diff_frac_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
      
    } else {
      # experimental & absolute
      dat %>%
        add_stat_dependency(confidence_limit = confidence_limit_1,
                            theoretical = FALSE,
                            fractional = FALSE) %>%
        add_stat_dependency(confidence_limit = confidence_limit_2,
                            theoretical = FALSE, 
                            fractional = FALSE) %>%
        select(Protein, Sequence, Start, End, diff_deut_uptake, err_diff_deut_uptake, paste0("valid_at_", confidence_limit_1), paste0("valid_at_", confidence_limit_2)) %>%
        mutate(diff_deut_uptake = round(diff_deut_uptake, 4),
               err_diff_deut_uptake = round(err_diff_deut_uptake, 4)) %>%
        arrange(Start, End) %>%
        rename("Diff Abs Value Exch" = diff_deut_uptake,
               "Err Diff Abs Value Exch" = diff_deut_uptake,
               "{column_name_cl1}" := paste0("valid_at_", confidence_limit_1),
               "{column_name_cl2}" := paste0("valid_at_", confidence_limit_2))
    }
  }
  
  
}


#' generate_differential_plot
#' 
#' @description Generates differential (Woods) plot with confidence values
#' based on supplied data and parameters.
#' 
#' @param dat produced by \code{\link{generate_differential_data_set}} function
#' @param theoretical \code{logical}, determines if values are theoretical
#' @param fractional \code{logical}, determines if values are fractional
#' @param confidence_limit ...
#' @param confidence_limit_2 ...
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_differential_plot

generate_differential_plot <- function(dat, 
                                       theoretical, 
                                       fractional,
                                       confidence_limit, 
                                       confidence_limit_2){ 
  
  if(theoretical){
    
    if(fractional){
      # theoretical & fractional  
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = TRUE,
                                                    fractional = TRUE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = TRUE,
                                                      fractional = TRUE)
      
      mutate(dat, colour = case_when(
        dat[["diff_theo_frac_deut_uptake"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_theo_frac_deut_uptake"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_theo_frac_deut_uptake"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_theo_frac_deut_uptake"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_theo_frac_deut_uptake, xend = End, yend = diff_theo_frac_deut_uptake, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_theo_frac_deut_uptake - err_diff_theo_frac_deut_uptake, ymax = diff_theo_frac_deut_uptake + err_diff_theo_frac_deut_uptake, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) + 
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-100, 100)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
      
    } else {
      # theoretical & absolute
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = TRUE,
                                                    fractional = FALSE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = TRUE,
                                                      fractional = FALSE)
      
      mutate(dat, colour = case_when(
        dat[["diff_theo_deut_uptake"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_theo_deut_uptake"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_theo_deut_uptake"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_theo_deut_uptake"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_theo_deut_uptake, xend = End, yend = diff_theo_deut_uptake, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_theo_deut_uptake - err_diff_theo_deut_uptake, ymax = diff_theo_deut_uptake + err_diff_theo_deut_uptake, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) + 
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
    }
    
  } else {
    
    if(fractional){
      # experimental & fractional
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = FALSE,
                                                    fractional = TRUE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = FALSE,
                                                      fractional = TRUE)
      
      mutate(dat, colour = case_when(
        dat[["diff_frac_deut_uptake"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_frac_deut_uptake"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_frac_deut_uptake"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_frac_deut_uptake"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_frac_deut_uptake, xend = End, yend = diff_frac_deut_uptake, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_frac_deut_uptake - err_diff_frac_deut_uptake, ymax = diff_frac_deut_uptake + err_diff_frac_deut_uptake, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) +
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-100, 100)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
      
    } else {
      # experimental & absolute
      interval <- calculate_confidence_limit_values(calc_dat = dat,
                                                    confidence_limit = confidence_limit,
                                                    theoretical = FALSE,
                                                    fractional = FALSE)
      
      interval_2 <- calculate_confidence_limit_values(calc_dat = dat,
                                                      confidence_limit = confidence_limit_2,
                                                      theoretical = FALSE,
                                                      fractional = FALSE)
      
      mutate(dat, colour = case_when(
        dat[["diff_deut_uptake"]] < interval_2[1] ~ "deepskyblue3",
        dat[["diff_deut_uptake"]] < interval[1] ~ "deepskyblue1",
        dat[["diff_deut_uptake"]] > interval_2[2] ~ "firebrick3",
        dat[["diff_deut_uptake"]] > interval[2] ~ "firebrick1",
        TRUE ~ "azure3")) %>%
        ggplot() +
        geom_segment(aes(x = Start, y = diff_deut_uptake, xend = End, yend = diff_deut_uptake, color = colour)) +
        geom_errorbar(aes(x = Med_Sequence, ymin = diff_deut_uptake - err_diff_deut_uptake, ymax = diff_deut_uptake + err_diff_deut_uptake, color = colour)) +
        geom_hline(yintercept = 0, linetype = "dotted", color = "green", size = .7) +
        geom_hline(aes(yintercept = interval[1], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "deepskyblue1", size = .7, show.legend = TRUE) + 
        geom_hline(aes(yintercept = interval[2], linetype = paste0(" Confidence interval ", confidence_limit*100, "% : ", round(interval[2], 4))), color = "firebrick1", size = .7, show.legend = FALSE) +
        geom_hline(aes(yintercept = interval_2[1], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "deepskyblue3", size = .7, show.legend = TRUE) +
        geom_hline(aes(yintercept = interval_2[2], linetype = paste0(" Confidence interval ", confidence_limit_2*100, "% : ", round(interval_2[2], 4))), color = "firebrick3", size = .7, show.legend = FALSE) +
        scale_linetype_manual(values = c("dashed", "dotdash")) + 
        scale_colour_identity() +
        scale_y_continuous(expand = c(0, 0), limits = c(-1, 1)) +
        theme(legend.title = element_blank(),
              legend.position = "bottom",
              legend.direction = "vertical") 
      
    }
  }
  
}