context("calculate_state_deuteration in test")

library(tidyr)
library(dplyr)

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

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

test_that("class is right",
          expect_is(calculate_state_uptake(dat, 
                                           protein = chosen_protein, 
                                           state = chosen_state,
                                           time_0 = chosen_time_0, 
                                           time_t = chosen_time,
                                           time_100 = chosen_time_100),
                    "data.frame"))



lapply(times, function(chosen_time){
  
  result_tmp <- calculate_state_uptake(dat,
                                       protein = chosen_protein,
                                       state = chosen_state,
                                       time_0 = chosen_time_0,
                                       time_t = chosen_time,
                                       time_100 = chosen_time_100,
                                       deut_part = deut_part)
  
  test_that("frac_deut_uptake is right",
            expect_equal(result_tmp %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(frac_deut_uptake) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["frac_deut_uptake"][[1]]))
  
  test_that("err_frac_deut_uptake is right",
            expect_equal(result_tmp %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(err_frac_deut_uptake) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["err_frac_deut_uptake"][[1]]))
  
  test_that("deut_uptake is right",
            expect_equal(result_tmp %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(deut_uptake) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["deut_uptake"][[1]]))
  
  test_that("err_deut_uptake is right",
            expect_equal(result_tmp %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(err_deut_uptake) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["err_deut_uptake"][[1]]))

  test_that("theo_frac_deut_uptake  is right",
            expect_equal(result_tmp %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(theo_frac_deut_uptake) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["theo_frac_deut_uptake"][[1]]))
  
  test_that("err_theo_frac_deut_uptake is right",
            expect_equal(result_tmp %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(err_theo_frac_deut_uptake) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["err_theo_frac_deut_uptake"][[1]]))
  
  test_that("theo_deut_uptake is right",
            expect_equal(result_tmp %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(theo_deut_uptake) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["theo_deut_uptake"][[1]]))
  
  test_that("err_theo_deut_uptake is right",
            expect_equal(result_tmp %>%
                           filter(Sequence == chosen_peptide) %>%
                           select(err_theo_deut_uptake) %>%
                           .[[1]],
                         ref_dat[ ref_dat[["Exposure"]] == chosen_time, ]["err_theo_deut_uptake"][[1]]))
})
