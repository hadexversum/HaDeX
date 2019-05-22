library(testthat)
library(HaDeX)

context("read_hdx in test")

test_that("doesn\'t works",
          expect_error(read_hdx("protein")))

test_that("file with , passes",
          expect_silent(read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))))

test_that("file with modification passes",
          expect_silent(read_hdx(system.file(package = "HaDeX", "HaDeX/data/S100A9_metale_NaCl.csv"))))

test_that("file with ; passes",
          expect_silent(read_hdx(system.file(package = "HaDeX", "HaDeX/data/Isu_poprawkadlugosci_a.csv"))))

test_that("xlsx file passes",
          expect_silent(read_hdx(system.file(package = "HaDeX", "HaDeX/data/161114_BETA_alpha_gamma.xlsx"))))
