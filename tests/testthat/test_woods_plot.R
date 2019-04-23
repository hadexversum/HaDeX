library(testthat)
library(HaDeX)
library(digest)
library(vdiffr)

context("woods_plot in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
calc_dat <- prepare_dataset(dat,
                            in_state_first = "CD160_0.001",
                            chosen_state_first = "CD160_1",
                            out_state_first = "CD160_1440",
                            in_state_second = "CD160_HVEM_0.001",
                            chosen_state_second = "CD160_HVEM_1",
                            out_state_second = "CD160_HVEM_1440")       

test_that("class is right",
          expect_is(woods_plot(calc_dat = calc_dat,
                               theoretical = TRUE, 
                               relative = FALSE),
                    "gg"))

test_that("md5 check 1",
          expect_equal(digest(as.character(woods_plot(calc_dat = calc_dat,
                                                      theoretical = TRUE,
                                                      relative = TRUE)[9]),
                              algo = "md5",
                              serialize = FALSE),
                       "2e88e4d4e7eb31db352e6bf2249920c2"))

test_that("md5 check 2",
          expect_equal(digest(as.character(woods_plot(calc_dat = calc_dat,
                                                           theoretical = FALSE,
                                                           relative = FALSE)[9]),
                              algo = "md5",
                              serialize = FALSE),
                       "12e3c600751338f2c2c03c4d3d386a39"))

wood_plot_1 <- woods_plot(calc_dat = calc_dat,
                          theoretical = FALSE, 
                          relative = TRUE)

vdiffr::expect_doppelganger("Woods Plot 1", wood_plot_1)

wood_plot_2 <- woods_plot(calc_dat = calc_dat,
                          theoretical = TRUE, 
                          relative = TRUE)

vdiffr::expect_doppelganger("Woods Plot 2", wood_plot_2)