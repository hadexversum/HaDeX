library(testthat)
library(HaDeX)

context("calculate_state_deuteration in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

test_that("class is right",
          expect_is(calculate_state_deuteration(dat, 
                                                protein = "db_CD160", 
                                                state = "CD160",
                                                time_in = 0.001, 
                                                time_chosen = 1,
                                                time_out = 144),
                    "data.frame"))