context("diff_deut_uptake calculations in test")

library(tidyr)
library(dplyr)

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))


################
## REFERENCES ##
################

chosen_protein <- "db_CD160"
chosen_states <- c("CD160", "CD160_HVEM")
chosen_time_0 <- 0.001
chosen_time_100 <- 1440
chosen_time <- 5
deut_part <- 1
chosen_confidence_level <-  0.98

chosen_peptide <- "INITSSASQEGTRLN"
ref_dat <- data.frame(Exposure = c(0.167, 1, 5, 25, 120),
                      diff_deut_uptake = c(0.31887150, 0.34128550, 0.54904750,  0.64388750,   0.10823500),
                      err_diff_deut_uptake = c(0.04604606, 0.09693827, 0.08253328,  0.05494542,   0.08564671),
                      diff_frac_deut_uptake = c(2.49775307, 2.65850667, 4.64333261,  5.51772139,   0.31699929),
                      err_diff_frac_deut_uptake = c(0.77092245, 1.16299771, 1.06350514,  0.91482812,   1.14952706),
                      diff_theo_frac_deut_uptake = c(2.26120002, 2.42014347, 3.89343738,  4.56597227,   0.76752229),
                      err_diff_theo_frac_deut_uptake = c(0.32652445, 0.68741429, 0.58526480,  0.38963213,   0.60734292),
                      diff_theo_deut_uptake = c(0.31887150, 0.34128550, 0.54904750,  0.64388750,   0.10823500),
                      err_diff_theo_deut_uptake = c(0.04604606, 0.09693827, 0.08253328,  0.05494542,   0.08564671))

times <- ref_dat[["Exposure"]]
deut_values <- colnames(ref_dat)[-1]

###########################
## CALCULATE_DIFF_UPTAKE ##
###########################

lapply(times, function(time){
  
  result_tmp <- calculate_diff_uptake(dat = dat, 
                                      protein = chosen_protein, 
                                      states = chosen_states, 
                                      time_0 = chosen_time_0, 
                                      time_t = time, 
                                      time_100 = chosen_time_100, 
                                      deut_part = deut_part)
  
  lapply(deut_values, function(deut_value){
    
    test_name <- paste0("calculate_diff_uptake-", time, "min-", deut_value)
    
    test_that(test_name,
              expect_equal(ref_dat[ref_dat[["Exposure"]] == time, deut_value],
                           result_tmp[result_tmp[["Sequence"]] == chosen_peptide & result_tmp[["Exposure"]] == time, deut_value][[1]]
              )
    )
    
  })
  
})

################################
## CREATE_DIFF_UPTAKE_DATASET ##
################################

lapply(times, function(time){
  
  result_tmp <- create_diff_uptake_dataset(dat = dat, 
                                           protein = chosen_protein, 
                                           state_1 = chosen_states[1], 
                                           state_2 = chosen_states[2],
                                           time_0 = chosen_time_0, 
                                           time_100 = chosen_time_100, 
                                           deut_part = deut_part)
  
  lapply(deut_values, function(deut_value){
    
    test_name <- paste0("create_diff_uptake_dataset-", time, "min-", deut_value)
    
    test_that(test_name,
              expect_equal(ref_dat[ref_dat[["Exposure"]] == time, deut_value],
                           result_tmp[result_tmp[["Sequence"]] == chosen_peptide & result_tmp[["Exposure"]] == time, deut_value][[1]]
              )
    )
    
  })
  
})

############################
## CREATE_VOLCANO_DATASET ##
############################

## TO DO test p value

dat_tmp <- create_volcano_dataset(dat = dat, 
                                  protein = chosen_protein,
                                  state_1 = chosen_states[1],
                                  state_2 = chosen_states[2],
                                  p_adjustment_method = "none",
                                  confidence_level = chosen_confidence_level)

lapply(times, function(time){
  
  test_name <- paste0("create_volcano_dataset-", time, "min")
  test_that("",
            expect_equal(dat_tmp[dat_tmp[["Sequence"]] == chosen_peptide & dat_tmp[["Exposure"]] == time, "D_diff"],
                         ref_dat[ref_dat[["Exposure"]] == time, "diff_deut_uptake"]
            )
  )
  
})
  
  



