library(testthat)
library(HaDeX)

context("quality_control in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

test_that("class is right",
          expect_is(create_quality_control_dataset(dat = dat,
                                    protein = "db_CD160", 
                                    state_first = "CD160",
                                    state_second = "CD160_HVEM", 
                                    time_t = 1, 
                                    time_0 = 0.001, 
                                    deut_part = 1),
                    "data.frame"))

test_that("size is right",
          expect_equal(length(colnames(create_quality_control_dataset(dat = dat,
                                                       protein = "db_CD160", 
                                                       state_first = "CD160",
                                                       state_second = "CD160_HVEM", 
                                                       time_t = 1, 
                                                       time_0 = 0.001, 
                                                       deut_part = 1))),
                       7))
