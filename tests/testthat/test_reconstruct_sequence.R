library(testthat)
library(HaDeX)
library(stringr)

context("reconstruct_sequence in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

test_that("example sequence is right length",
          expect_equal(stringr::str_length(reconstruct_sequence(dat)), 
                       132))

test_that("class is right",
          expect_is(reconstruct_sequence(dat), 
                    "character"))

