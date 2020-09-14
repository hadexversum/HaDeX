library(testthat)
library(HaDeX)
library(checkmate)

context("calculate_kinetics in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

test_that("class is right", {
  kin_1 <- calculate_kinetics(dat, 
                             protein = "db_CD160",
                             sequence = "INITSSASQEGTRLN", 
                             state = "CD160",
                             start = 1, 
                             end = 15,
                             time_0 = 0.001, 
                             time_100 = 1440)
  expect_data_frame(kin_1, ncols = 15, nrows = 5)
})
