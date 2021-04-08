#' generate_vulcano_data
#' 
#' Generates data for vulcano plot
#' 
#' @param dat ...
#' @param protein ...
#' @param states ...
#' 
#' @export generate_vulcano_data

generate_vulcano_data <- function(dat,
                                  protein = unique(dat[["Protein"]])[1],
                                  states = unique(dat[["State"]])[1:2]){
  
  proton_mass <- 1.00727647
  
  tmp_dat <- dat %>%
    ## mass calculation
    filter(Protein == protein) %>%
    select(Sequence, Start, End, State, Exposure, File, z, Inten, Center) %>%
    mutate(exp_mass = Center*z - z*proton_mass) %>%
    group_by(Sequence, Start, End, State, Exposure, File) %>%
    summarize(avg_exp_mass = weighted.mean(exp_mass, Inten, na.rm = TRUE)) %>%
    ungroup(.) %>%
    ## end of mass calculation
    # filter(Exposure != 0, Exposure != 0.001) %>%
    group_by(Sequence, Start, End, State, Exposure) %>%
    summarize(avg_mass = mean(avg_exp_mass),
              err_avg_mass = sd(avg_exp_mass)/sqrt(length(Exposure)),
              masses = list(avg_exp_mass)) %>%
    arrange(Start, End) 
  
  
  state_1 <- states[1]
  state_2 <- states[2]
  
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
  
  vul_dat <- merge(tmp_dat_1, tmp_dat_2, by = c("Sequence", "Start", "End", "Exposure"))
  
  res_vulcano <- data.frame()
  
  lapply(1:nrow(vul_dat), function(i){
    
    diff_d <- vul_dat[i, "avg_mass_1"] - vul_dat[i, "avg_mass_2"]
    uncertainty <- sqrt(vul_dat[i, "err_avg_mass_1"]^2 + vul_dat[i, "err_avg_mass_2"]^2 )
    
    st_1 <- vul_dat[i, "masses_1"][[1]]
    st_2 <- vul_dat[i, "masses_2"][[1]]
    
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
    
    
    res_vulcano <<- bind_rows(res_vulcano, data.frame(Sequence = vul_dat[i, "Sequence"],
                                                      Exposure = vul_dat[i, "Exposure"],
                                                      D_diff = diff_d,
                                                      P_value = p_value,
                                                      Uncertainty = uncertainty))
    
  })
  
  res_vulcano %>%
    filter(P_value!=-1) %>%
    mutate(log_p_value = -log(P_value)) %>%
    select(-P_value)
  
}

#' generate_vulcano_plot
#' 
#' Generates vulcano plot based on suplied vulcano data
#' 
#' @param vul_data ...
#' 
#' @export generate_vulcano_plot
 
generate_vulcano_plot <- function(vul_data, state_1 = "", state_2 = "") {
  
  x_max <- ceiling(max(abs(vul_data[["D_diff"]])))
  y_max <- ceiling(max(vul_data[["log_p_value"]])) + 2
  
  ggplot(vul_data, aes(x = D_diff, y = log_p_value)) + 
    geom_jitter() + 
    geom_errorbar(aes(xmin = D_diff - Uncertainty, xmax = D_diff + Uncertainty), alpha = 0.2) + 
    coord_cartesian(xlim = c(-x_max, x_max), ylim = c(0, y_max), expand = FALSE) + 
    labs(title = paste0("Vulcano Plot ", state_1, " " , state_2),  
         x = "Mass difference [Da]",
         y = "-log(P value)")
  
}


