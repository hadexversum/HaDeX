context("deut_uptake calculations in test")

library(tidyr)
library(dplyr)

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))


################
## REFERENCES ##
################

chosen_protein <- "db_CD160"
chosen_state <- "CD160"
chosen_time_0 <- 0.001
chosen_time_100 <- 1440
chosen_time <- 5
deut_part <- 1

chosen_peptide <- "INITSSASQEGTRLN"
ref_dat <- data.frame(Exposure = c(0.167, 1, 5, 25, 120),
                      deut_uptake = c(8.003550333, 8.767922333, 9.115434833, 9.705116833, 9.945452833),
                      err_deut_uptake = c(0.0282742947, 0.0556657664, 0.0529310195, 0.0496445668, 0.0794090986),
                      frac_deut_uptake = c(76.85614645, 84.19622478, 87.53330276, 93.19587553, 95.50376366),
                      err_frac_deut_uptake = c(0.383193112, 0.611138265, 0.594304636, 0.5786019863, 0.8332955382),
                      theo_frac_deut_uptake = c(56.35082959, 61.77118851, 64.23548925, 68.41707634, 70.12136088),
                      err_theo_frac_deut_uptake = c(0.2005003133, 0.3947403009, 0.375347506, 0.3520424223, 0.5631103913),
                      theo_deut_uptake = c(7.94652106, 8.71089306, 9.05840556, 9.64808756, 9.88842356),
                      err_theo_deut_uptake = c(0.0282742947, 0.05566576636, 0.05293101952, 0.04964456678, 0.07940909861))

times <- ref_dat[["Exposure"]]
deut_values <- colnames(ref_dat)[-1]

states <- unique(dat[["State"]])

############################
## CALCULATE_STATE_UPTAKE ##
############################

test_that("class is right",
          expect_is(calculate_state_uptake(dat, 
                                           protein = chosen_protein, 
                                           state = chosen_state,
                                           time_0 = chosen_time_0, 
                                           time_t = chosen_time,
                                           time_100 = chosen_time_100),
                    "data.frame"))

lapply(times, function(time){
  
  result_tmp <- calculate_state_uptake(dat,
                                       protein = chosen_protein,
                                       state = chosen_state,
                                       time_0 = chosen_time_0,
                                       time_t = time,
                                       time_100 = chosen_time_100,
                                       deut_part = deut_part)
  
  lapply(deut_values, function(deut_value){
    
    test_name <- paste0("calculate_state_uptake-", time, "min-", deut_value)
    
    test_that(test_name,
              expect_equal(ref_dat[ref_dat[["Exposure"]] == time, deut_value],
                           result_tmp[result_tmp[["Sequence"]] == chosen_peptide & result_tmp[["Exposure"]] == time, deut_value][[1]]
              )
    )
    
  })
  
})


#################################
## CREATE_STATE_UPTAKE_DATASET ##
#################################
## chiclet
## butterfly

result_tmp <- create_state_uptake_dataset(dat = dat, 
                                          protein = chosen_protein, 
                                          state = chosen_state,
                                          time_0 = chosen_time_0,
                                          time_100 = chosen_time_100,
                                          deut_part = deut_part)

lapply(times, function(time){
  
  lapply(deut_values, function(deut_value){
    
    test_name <- paste0("create_state_uptake_dataset-", time, "min-", deut_value)

    test_that(test_name, 
              expect_equal(ref_dat[ref_dat[["Exposure"]] == time, deut_value],
                           result_tmp[result_tmp[["Sequence"]] == chosen_peptide & result_tmp[["Exposure"]] == time, deut_value][[1]]
              )
    )
    
  })
  
})

###########################
## CREATE_UPTAKE_DATASET ##
########################### 

result_tmp <- create_uptake_dataset(dat = dat,
                                    protein = chosen_protein,
                                    states = unique(dat[["State"]]),
                                    time_0 = chosen_time_0,
                                    time_100 = chosen_time_100,
                                    deut_part = deut_part)


lapply(times, function(time){

  lapply(deut_values, function(deut_value){

    test_name <- paste0("create_uptake_dataset-", time, "min-", deut_value)

    test_that(test_name,
              expect_equal(ref_dat[ref_dat[["Exposure"]] == time, deut_value],
                           result_tmp[result_tmp[["State"]] == chosen_state & result_tmp[["Sequence"]] == chosen_peptide & result_tmp[["Exposure"]] == time, deut_value][[1]]
              )
    )

  })

})


#####################################
## CREATE_STATE_COMPARISON_DATASET ##
#####################################

lapply(times, function(time){
  
  lapply(deut_values, function(deut_value){
    
    result_tmp <- create_state_comparison_dataset(dat = dat,
                                                  protein = chosen_protein,
                                                  states = states, 
                                                  time_0 = chosen_time_0,
                                                  time_t = time,
                                                  time_100 = chosen_time_100,
                                                  deut_part = deut_part)
    
    test_name <- paste0("create_state_comparison_dataset-", time, "min-", deut_value)
    
    test_that(test_name, 
              expect_equal(
                ref_dat[ref_dat[["Exposure"]] == time, deut_value],
                result_tmp[result_tmp[["State"]] == chosen_state & result_tmp[["Sequence"]] == chosen_peptide, deut_value][[1]]
              )
    )
    
  })
  
})

############################
## CREATE_CONTROL_DATASET ##
############################

dat_tmp <- create_control_dataset(dat = dat,
                                     control_protein = chosen_protein,
                                     control_state = chosen_state,
                                     control_exposure = chosen_time_100)

test_that("maximal exchange control added",
          expect_true(99999 %in% unique(dat_tmp[["Exposure"]])))


lapply(times, function(time){
  
  result_tmp <- calculate_state_uptake(dat_tmp,
                                       protein = chosen_protein,
                                       state = chosen_state,
                                       time_0 = chosen_time_0,
                                       time_t = time,
                                       time_100 = 99999,
                                       deut_part = deut_part)
  
  lapply(deut_values, function(deut_value){
    
    test_name <- paste0("calculate_state_uptake with control -", time, "min-", deut_value)
    
    test_that(test_name,
              expect_equal(ref_dat[ref_dat[["Exposure"]] == time, deut_value],
                           result_tmp[result_tmp[["Sequence"]] == chosen_peptide & result_tmp[["Exposure"]] == time, deut_value][[1]]
              )
    )
    
  })
  
})
