library(testthat)
library(HaDeX)

context("quality_control in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

result <- create_quality_control_dataset(dat = dat,
                                         protein = "db_CD160", 
                                         state_first = "CD160",
                                         state_second = "CD160_HVEM", 
                                         time_t = 1, 
                                         time_0 = 0.001, 
                                         deut_part = 1)

test_that("class is right",
          expect_is(result,
                    "data.frame"))

test_that("size is right",
          expect_equal(length(colnames(result)),
                       7))
