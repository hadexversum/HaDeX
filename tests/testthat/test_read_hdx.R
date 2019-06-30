library(testthat)
library(HaDeX)

context("read_hdx in test")

test_that("doesn't work",
          expect_error(read_hdx("protein")))

test_that("file with , passes",
          expect_silent(read_hdx("https://raw.githubusercontent.com/hadexversum/HDX-data/master/KD_180110_CD160_HVEM.csv")))

test_that("file with modification passes",
          expect_silent(read_hdx("https://raw.githubusercontent.com/hadexversum/HDX-data/master/S100A9_metale_NaCl.csv")))

test_that("file with ; passes",
          expect_silent(read_hdx("https://raw.githubusercontent.com/hadexversum/HDX-data/master/Isu_poprawkadlugosci_a.csv")))

# test_that("xlsx file passes",
#           expect_silent(read_hdx("https://raw.githubusercontent.com/hadexversum/HDX-data/master/161114_BETA_alpha_gamma.xlsx")))

test_that("dynamx 2 file passes",
          expect_silent(read_hdx("https://raw.githubusercontent.com/hadexversum/HDX-data/master/lysenin.csv")))
