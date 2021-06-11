library(testthat)
library(HaDeX)
library(digest)
library(vdiffr)

context("woods_plot in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

