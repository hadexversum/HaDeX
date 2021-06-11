library(testthat)
library(HaDeX)
library(checkmate)

context("calculate_kinetics in test")

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

