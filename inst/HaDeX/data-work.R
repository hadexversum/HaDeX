library(HaDeX)
library(shiny)
library(reshape2)
library(gsubfn)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(latex2exp)

#########################################

amino_prop <- read.csv("./data/amino_prop.csv")

amino_prop <- amino_prop %>%
  mutate(charge = factor(charge, levels = c("-1", "0", "1")))

file_req <- data.frame(
  Name = c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
  Type = c("Character", "Integer", "Integer", "Character", "Logic", "Logic", "Numeric", "Numeric", "Character", "Numeric", "Character", "Integer", "Numeric", "Numeric", "Numeric")
)

proton_mass <- 1.00727647
