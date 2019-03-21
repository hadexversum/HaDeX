library(testthat)
library(HaDeX)

context("comparison_plot in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
calc_dat <- prepare_dataset(dat,
                            in_state_first = "CD160_0.001",
                            chosen_state_first = "CD160_1",
                            out_state_first = "CD160_1440",
                            in_state_second = "CD160_HVEM_0.001",
                            chosen_state_second = "CD160_HVEM_1",
                            out_state_second = "CD160_HVEM_1440")                             

test_that("class is right",
          expect_is(comparison_plot(calc_dat = calc_dat,
                                    theoretical = TRUE,
                                    state_first = "CD160",
                                    state_second = "CD160_HVEM"),
                    "gg"))