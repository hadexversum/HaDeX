library(testthat)
library(HaDeX)

context("quality_control in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

test_that("class is right",
          expect_is(quality_control(dat = dat,
                          state_first = "CD160",
                          state_second = "CD160_HVEM", 
                          chosen_time = 1, 
                          in_time = 0.001, 
                          relative = FALSE),
                    "data.frame"))

test_that("size is right",
          expect_equal(length(colnames(quality_control(dat = dat,
                                 state_first = "CD160",
                                 state_second = "CD160_HVEM", 
                                 chosen_time = 1, 
                                 in_time = 0.001, 
                                 relative = TRUE))),
                       13))
