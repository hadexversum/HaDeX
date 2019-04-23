library(testthat)
library(HaDeX)

context("add_stat_dependeny in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
calc_dat <- prepare_dataset(dat,
                            in_state_first = "CD160_0.001",
                            chosen_state_first = "CD160_1",
                            out_state_first = "CD160_1440",
                            in_state_second = "CD160_HVEM_0.001",
                            chosen_state_second = "CD160_HVEM_1",
                            out_state_second = "CD160_HVEM_1440")   

test_that("class is right",
          expect_is(add_stat_dependency(calc_dat,
                                         confidence_limit = 0.98,
                                         theoretical = FALSE,
                                         relative = TRUE),
                    "data.frame"))

test_that("added columns",
          expect_equal(length(add_stat_dependency(calc_dat,
                                                  confidence_limit = 0.98,
                                                  theoretical = FALSE,
                                                  relative = TRUE)),
                       29))


test_that("attribute added", 
          expect_equal(attr(add_stat_dependency(calc_dat,
                                                confidence_limit = 0.98,
                                                theoretical = FALSE,
                                                relative = TRUE),
                            "confidence_limit_at_0.98"),
                       calculate_confidence_limit_values(calc_dat, 
                                                         confidence_limit = 0.98,
                                                         theoretical = FALSE, 
                                                         relative = TRUE)))