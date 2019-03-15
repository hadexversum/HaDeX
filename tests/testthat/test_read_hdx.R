library(testthat)
library(HaDeX)

context("read_hdx in test")

filename <- system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv")

test_that("doesn\'t works",
          expect_error(read_hdx("protein")))
