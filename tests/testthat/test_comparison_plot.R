library(testthat)
library(HaDeX)
library(digest)
library(vdiffr)

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

test_that("md5 check 1",
          expect_equal(digest(as.character(comparison_plot(calc_dat = calc_dat,
                                                           theoretical = TRUE,
                                                           relative = TRUE,
                                                           state_first = "CD160",
                                                           state_second = "CD160_HVEM")[9]),
                              algo = "md5",
                              serialize = FALSE),
                       "ec84f005f2fb2aad767aa73b70e0b492"))

test_that("md5 check 2",
          expect_equal(digest(as.character(comparison_plot(calc_dat = calc_dat,
                                                           theoretical = FALSE,
                                                           relative = FALSE,
                                                           state_first = "CD160",
                                                           state_second = "CD160_HVEM")[9]),
                              algo = "md5",
                              serialize = FALSE),
                       "b7ef9ee469a37813d0f393ef89ce1b31"))

comparison_plot_1 <- comparison_plot(calc_dat = calc_dat,
                                     theoretical = TRUE,
                                     relative = FALSE, 
                                     state_first = "CD160",
                                     state_second = "CD160_HVEM")

vdiffr::expect_doppelganger("Comparison Plot 1", comparison_plot_1)

comparison_plot_2 <- comparison_plot(calc_dat = calc_dat,
                                     theoretical = FALSE,
                                     relative = TRUE, 
                                     state_first = "CD160",
                                     state_second = "CD160_HVEM")


vdiffr::expect_doppelganger("Comparison Plot 2", comparison_plot_2)