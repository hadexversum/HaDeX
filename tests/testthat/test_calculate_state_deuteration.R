context("calculate_state_deuteration in test")

library(tidyr)
library(dplyr)

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

chosen_protein <- "db_CD160"
chosen_state <- "CD160"
chosen_time_0 <- 0.001
chosen_time_100 <- 1440
chosen_time <- 5

chosen_peptide <- "INITSSASQEGTRLN"
ref_dat <- data.frame(Exposure = c(0.167, 1, 5, 25, 120),
                      deut_uptake = c(8.003550333, 8.767922333, 9.115434833, 9.705116833, 9.945452833),
                      err_deut_uptake = c(0.0282742947, 0.0556657664, 0.0529310195, 0.0496445668, 0.0794090986),
                      frac_deut_uptake = c(76.85614645, 84.19622478, 87.53330276, 93.19587553, 95.50376366),
                      err_frac_deut_uptake = c(0.383193112, 0.611138265, 0.594304636, 0.5786019863, 0.8332955382))

times <- ref_dat[["Exposure"]]

test_that("class is right",
          expect_is(calculate_state_deuteration(dat, 
                                                protein = chosen_protein, 
                                                state = chosen_state,
                                                time_in = chosen_time_0, 
                                                time_chosen = chosen_time,
                                                time_out = chosen_time_100),
                    "data.frame"))



lapply(times, function(chosen_time){
  
  test_that("frac_deut_uptake is right",
            expect_equal(calculate_state_deuteration(dat,
                                                     protein = chosen_protein,
                                                     state = chosen_state,
                                                     time_in = chosen_time_0,
                                                     time_chosen = chosen_time,
                                                     time_out = chosen_time_100) %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(frac_exch_state) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["frac_deut_uptake"][[1]]))
  
  
})

lapply(times, function(chosen_time){
  
  test_that("err_frac_deut_uptake is right",
            expect_equal(calculate_state_deuteration(dat,
                                                     protein = chosen_protein,
                                                     state = chosen_state,
                                                     time_in = chosen_time_0,
                                                     time_chosen = chosen_time,
                                                     time_out = chosen_time_100) %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(err_frac_exch_state) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["err_frac_deut_uptake"][[1]]))
  
  
})

lapply(times, function(chosen_time){
  
  test_that("deut_uptake is right",
            expect_equal(calculate_state_deuteration(dat,
                                                     protein = chosen_protein,
                                                     state = chosen_state,
                                                     time_in = chosen_time_0,
                                                     time_chosen = chosen_time,
                                                     time_out = chosen_time_100) %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(abs_frac_exch_state) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["deut_uptake"][[1]]))
  
  
})

lapply(times, function(chosen_time){
  
  test_that("deut_uptake is right",
            expect_equal(calculate_state_deuteration(dat,
                                                     protein = chosen_protein,
                                                     state = chosen_state,
                                                     time_in = chosen_time_0,
                                                     time_chosen = chosen_time,
                                                     time_out = chosen_time_100) %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(err_abs_frac_exch_state) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["err_deut_uptake"][[1]]))
  
  
})

# 
# ref_dat[chosen_time, ]["frac_deut_uptake"][[1]]
# 
# 
# calculate_state_deuteration(dat,
#                             protein = chosen_protein,
#                             state = chosen_state,
#                             time_in = chosen_time_0,
#                             time_chosen = chosen_time,
#                             time_out = chosen_time_100) %>%
#   filter(Sequence == chosen_peptide) %>%
#   select(frac_exch_state) %>%
#   .[[1]]


## testthat::test_file("tests/testthat/test_calculate_state_deuteration.R")