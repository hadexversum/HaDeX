library(testthat)
library(HaDeX)
library(vdiffr)

context("plot_coverage in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

test_that("class is right",
          expect_is(plot_coverage(dat, states = "CD160_HVEM"),
                    "gg"))

# graphic_overlapping_1 <- plot_coverage(dat, "CD160_HVEM")
# 
# vdiffr::expect_doppelganger("Graphic Overlapping Plot 1", graphic_overlapping_1)
