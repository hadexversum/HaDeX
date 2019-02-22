library(HaDeX)
library(shiny)
library(reshape2)
library(gsubfn)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(latex2exp)
library(DT)

#########################################

amino_prop <- read.csv("./data/amino_prop.csv")

amino_prop <- amino_prop %>%
  mutate(charge = factor(charge, levels = c("-1", "0", "1")))

file_req <- data.frame(
  Name = c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
  Type = c("Character", "Integer", "Integer", "Character", "Logic", "Logic", "Numeric", "Numeric", "Character", "Numeric", "Character", "Integer", "Numeric", "Numeric", "Numeric")
)

proton_mass <- 1.00727647

deuteration_mass <- 2.0141 - 1.008

dt_format <- function(dat) {
  
  datatable(data = dat,
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10, dom = "tBip", autoWidth = TRUE, buttons = c("excel", "pdf")),
            filter = "bottom",
            rownames = FALSE)
  
}
