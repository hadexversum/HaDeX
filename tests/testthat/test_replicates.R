# covr::file_coverage("R/replicates.R", test_files = "tests/testthat/test_replicates.R")

context("replicates in test")

library(tidyr)
library(vdiffr)

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

rep_mass_dat <- calculate_exp_masses_per_replicate(dat)

###################################
## show_peptide_mass_measurement ##
###################################

test_that("show_peptide_mass_measurement class is right",
          expect_is(
            show_peptide_mass_measurement(rep_mass_dat),
            "data.frame"
          )
)

test_that("show_peptide_mass_measurement colnames",
          expect_equal(
            colnames(show_peptide_mass_measurement(rep_mass_dat)),
            c("Protein", "Sequence", "Start", "End", "Exposure", "State", "File", "Mass")
          )
)

#####################################
## show_peptide_charge_measurement ##
#####################################

test_that("show_peptide_charge_measurement class is right",
          expect_is(
            show_peptide_charge_measurement(dat),
            "data.frame"
          )
)

test_that("show_peptide_charge_measurement colnames",
          expect_equal(
            colnames(show_peptide_charge_measurement(dat)),
            c("Protein", "Sequence", "Start", "End", "State", "Exposure",  "File", "z")
          )
)

##############################
## create_replicate_dataset ##
##############################

rep_dat <- create_replicate_dataset(dat)

test_that("create_replicate_dataset class is right",
          expect_is(
            rep_dat,
            "data.frame"
          )
)

test_that("create_replicate_dataset colnames",
          expect_equal(
            colnames(rep_dat),
            c("Sequence", "Exposure", "Start", "End", "ID", "n")
          )
)

test_that("create_replicate_dataset dimention",
          expect_equal(
            dim(rep_dat),
            c(328, 6)
          )
          
)

ref_dat <- structure(list(Sequence = c("INITSSASQEGTRLN", "INITSSASQEGTRLN", 
                                       "INITSSASQEGTRLN", "INITSSASQEGTRLN", 
                                       "INITSSASQEGTRLN", "INITSSASQEGTRLN", 
                                       "INITSSASQEGTRLN", "INITSSASQEGTRLN"), 
                          Exposure = c(0, 0.001,  0.167, 1, 5, 25, 120, 1440), 
                          Start = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                          End = c(15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L), 
                          ID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                          n = c(1L, 1L, 4L, 4L, 4L, 4L, 4L, 4L)), 
                     row.names = c(NA, -8L), 
                     class = c("tbl_df", "tbl", "data.frame"), 
                     state = "CD160")

test_that("create_replicate_dataset values",
          expect_equal(
            data.frame(ref_dat),
            data.frame(rep_dat[rep_dat[["Sequence"]] == "INITSSASQEGTRLN", ])
          )
)

###################################
## show_replicate_histogram_data ##
###################################

test_that("class is right",
          expect_is(
            show_replicate_histogram_data(rep_dat),
            "data.frame"
          )
)

test_that("show_replicate_histogram_data colnames",
          expect_equal(
            colnames(show_replicate_histogram_data(rep_dat)),
            c("Sequence", "Exposure", "Start", "End", "ID",  "n")
          )
)

###################################
## plot_peptide_mass_measurement ##
###################################

test_that("plot_peptide_mass_measurement class is right",
          expect_is(
            plot_peptide_mass_measurement(dat),
            "ggplot"
          )
)

expect_doppelganger(
  "Peptide Mass Measurement Plot",
  plot_peptide_mass_measurement(dat)
)

#####################################
## plot_peptide_charge_measurement ##
#####################################

test_that("plot_peptide_charge_measurement class is right",
          expect_is(
            plot_peptide_charge_measurement(dat),
            "ggplot"
          )
)

expect_doppelganger(
  "Peptide Charge Measurement Plot",
  plot_peptide_charge_measurement(dat)
)

##############################
## plot_replicate_histogram ##
##############################

test_that("plot_replicate_histogram class is right",
          expect_is(
            plot_replicate_histogram(rep_dat),
            "ggplot"
          )
)

expect_doppelganger(
  "Repliates histogram - all time",
  plot_replicate_histogram(rep_dat)
)

expect_doppelganger(
  "Repliates histogram - one time",
  plot_replicate_histogram(rep_dat[rep_dat[["Exposure"]] == 5, ])
)

