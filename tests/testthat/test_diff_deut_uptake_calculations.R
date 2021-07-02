context("diff_deut_uptake calculations in test")

library(tidyr)
library(dplyr)

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))


################
## REFERENCES ##
################

chosen_protein <- "db_CD160"
chosen_states <- c("CD160", "CD160_HVEM")
chosen_time_0 <- 0.001
chosen_time_100 <- 1440
chosen_time <- 5
deut_part <- 1

chosen_peptide <- "INITSSASQEGTRLN"
ref_dat <- data.frame()

## calculate_diff_uptake  

## create_diff_uptake_dataset 

## create_volcano_dataset 