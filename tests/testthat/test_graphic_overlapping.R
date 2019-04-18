library(testthat)
library(HaDeX)

context("graphic_overlapping in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

test_that("class is right",
          expect_is(graphic_overlapping(dat, "CD160_HVEM"),
                    "gg"))