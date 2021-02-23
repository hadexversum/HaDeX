## ---- echo = FALSE, message = FALSE, results='asis'---------------------------
library(HaDeX)
library(ggplot2)
library(knitr)
library(DT)
library(dplyr)
opts_chunk$set(fig.width = 7, fig.height = 5)

## ----echo=FALSE,results='asis'------------------------------------------------
read.csv2("comparison.csv") %>% 
  datatable(options = list(dom = "t", ordering = FALSE, paging = FALSE), rownames = FALSE, style = "bootstrap") %>%
  formatStyle(c("MSTools", "MEMHDX", "Deuteros", "HaDeX"), backgroundColor = styleEqual(c("Yes", "No"), c("#00BFFF", "#FF8C91")))

## ----warning=FALSE, message=FALSE, echo = FALSE-------------------------------

datatable(
  data = data.frame("Column Name" = c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", 
                             "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
           "Column Type" = c("Character", "Integer", "Integer", "Character", "Logic", "Logic", "Numeric", 
                             "Numeric", "Character", "Numeric", "Character", "Integer", "Numeric", "Numeric", "Numeric")),
  rownames = FALSE, style = "bootstrap",
  list(dom = "t", ordering = FALSE, paging = FALSE, autoWidth = TRUE))


## ----warning=FALSE------------------------------------------------------------

dat <- read_hdx(system.file(package = "HaDeX", 
                            "HaDeX/data/KD_190304_Nucb2_EDTA_CaCl2_test02_clusterdata.csv"))


## ----warning=FALSE, message=FALSE, echo=FALSE---------------------------------

dat_temp <- read.csv(system.file(package = "HaDeX", 
                            "HaDeX/data/KD_190304_Nucb2_EDTA_CaCl2_test02_clusterdata.csv"))

dat_temp %>% 
  filter(File == "KD_190119_gg_Nucb2_CaCl2_10s_01", Sequence == "KQFEHLNHQNPDTFEPKDLDML", Exposure == 0.167) %>%
  select(Sequence, File, z, RT, Inten, Center)


## ----warning=FALSE------------------------------------------------------------

reconstruct_sequence(dat)


## ----warning=FALSE, eval=FALSE------------------------------------------------
#  
#  plot_coverage(dat, chosen_state = "gg_Nucb2_CaCl2")
#  
#  plot_position_frequency(dat, chosen_state = "gg_Nucb2_CaCl2")
#  

## ----warning=FALSE,echo=FALSE, eval=FALSE-------------------------------------
#  example_qc <- rbind(data.frame(x = c(10, 25, 60, 1440),
#                                 y = c(0.008, 0.0075, 0.007, 0.0065),
#                                 type = "Uncertainty decreases too slowly\nExperiment should be prolonged",
#                                 Assessment = "Alter experimental settings"),
#                      data.frame(x = c(10, 25, 60, 1440),
#                                 y = c(0.008, 0.001, 0.001, 0.001),
#                                 type = "Uncertainty decreases too quickly\nExperiment should have more early timepoints",
#                                 Assessment = "Alter experimental settings"),
#                      data.frame(x = c(10, 25, 60, 1440),
#                                 y = c(0.008, 0.004, 0.003, 0.001),
#                                 type = "Uncertainty decreases properly",
#                                 Assessment = "Experiment conducted properly"))
#  ggplot(example_qc, aes(x = x, y = y, color = Assessment)) +
#    geom_line() +
#    geom_point() +
#    facet_wrap(~ type, ncol = 1) +
#    theme_bw() +
#    theme(legend.position = "bottom")

## ----warning=FALSE------------------------------------------------------------
library(HaDeX)

# file import
dat_1 <- read_hdx(system.file(package = "HaDeX", 
                              "HaDeX/data/KD_180110_CD160_HVEM.csv"))

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  reconstruct_sequence(dat_1)
#  
#  plot_position_frequency(dat_1, chosen_state = "CD160")

