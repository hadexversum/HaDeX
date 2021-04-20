#' generate_volcano_data
#' 
#' Generates data set for volcano plot
#' 
#' @param dat ...
#' @param protein ...
#' @param states ...
#' 
#' @export generate_volcano_dataset

generate_volcano_dataset <- function(dat,
                                     protein = unique(dat[["Protein"]])[1],
                                     state_1 = unique(dat[["State"]])[1],
                                     state_2 = unique(dat[["State"]])[2]){
  
  proton_mass <- 1.00727647
  
  tmp_dat <- dat %>%
    calculate_exp_masses_per_replicate(.) %>%
    group_by(Sequence, Start, End, State, Exposure) %>%
    summarize(avg_mass = mean(avg_exp_mass),
              err_avg_mass = sd(avg_exp_mass)/sqrt(length(Exposure)),
              masses = list(avg_exp_mass)) %>%
    arrange(Start, End) 
  
  tmp_dat_1 <- tmp_dat %>%
    filter(State == state_1) %>%
    rename(avg_mass_1 = avg_mass,
           err_avg_mass_1 = err_avg_mass, 
           masses_1 = masses) %>%
    ungroup(.) %>%
    select(-State)
  
  tmp_dat_2 <- tmp_dat %>%
    filter(State == state_2) %>%
    rename(avg_mass_2 = avg_mass,
           err_avg_mass_2 = err_avg_mass,
           masses_2 = masses) %>%
    ungroup(.) %>%
    select(-State)
  
  vol_dat <- merge(tmp_dat_1, tmp_dat_2, by = c("Sequence", "Start", "End", "Exposure"))
  
  res_volcano <- data.frame()
  
  lapply(1:nrow(vol_dat), function(i){
    
    diff_d <- vol_dat[i, "avg_mass_1"] - vol_dat[i, "avg_mass_2"]
    uncertainty <- sqrt(vol_dat[i, "err_avg_mass_1"]^2 + vol_dat[i, "err_avg_mass_2"]^2 )
    
    st_1 <- vol_dat[i, "masses_1"][[1]]
    st_2 <- vol_dat[i, "masses_2"][[1]]
    
    if(length(st_1)!=length(st_2)){
      len = min(length(st_1), length(st_2))
      st_1 <- st_1[1:len]
      st_2 <- st_2[1:len]
    }
    
    if(length(st_1) == 1 & length(st_2) == 1){
      p_value = -1
    } else {
      p_value <- t.test(st_1, st_2)$p.value
    }
    
    
    res_volcano <<- bind_rows(res_volcano, data.frame(Sequence = vol_dat[i, "Sequence"],
                                                      Exposure = vol_dat[i, "Exposure"],
                                                      D_diff = diff_d,
                                                      P_value = p_value,
                                                      Uncertainty = uncertainty,
                                                      Start = vol_dat[i, "Start"],
                                                      End = vol_dat[i, "End"]))
    
  })
  
  res_volcano %>%
    filter(P_value!=-1) %>%
    mutate(log_p_value = -log(P_value)) %>%
    select(Sequence, Start, End, Exposure, D_diff, Uncertainty, log_p_value)
  
}

#' generate_volcano_plot
#' 
#' Generates volcano plot based on supplied volcano data
#' 
#' @param vol_data ...
#' @param state_1 ...
#' @param state_2 ...
#' @param adjust_axes ...
#' 
#' @export generate_volcano_plot
 
generate_volcano_plot <- function(vol_data, 
                                  state_1 = "", 
                                  state_2 = "",
                                  adjust_axes = TRUE) {
  
  volcano_plot <- ggplot(vol_data, aes(x = D_diff, y = log_p_value)) + 
    geom_point() + 
    geom_errorbar(aes(xmin = D_diff - Uncertainty, xmax = D_diff + Uncertainty), alpha = 0.2) + 
    labs(title = paste0("Volcano Plot ", state_1, " " , state_2),  
         x = "Mass difference [Da]",
         y = "-log(P value)")
  
  if(adjust_axes){
    
    x_max <- ceiling(max(abs(vol_data[["D_diff"]])))
    y_max <- ceiling(max(vol_data[["log_p_value"]])) + 2
    
    volcano_plot + 
      coord_cartesian(xlim = c(-x_max, x_max), ylim = c(0, y_max), expand = FALSE) 
    
  }
  
}

#' generate_volcano_data
#'
#' 
#' 
#' @param vol_data ...
#'
#' @export generate_volcano_data

generate_volcano_data <- function(vol_data){
  
  vol_data %>%
    mutate(D_diff  = round(D_diff , 4),
           Uncertainty = round(Uncertainty, 4),
           log_p_value = round(log_p_value, 4)) %>%
    arrange(Exposure) %>%
    rename("Deuterium uptake difference" = D_diff , 
           "-log(P value)" = log_p_value)
  
}
