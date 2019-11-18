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
library(rmarkdown)
library(shinycssloaders)
library(shinyhelper)

#########################################

amino_prop <- read.csv("./data/amino_prop.csv")

amino_prop <- amino_prop %>%
  mutate(charge = factor(charge, levels = c("-1", "0", "1")))

file_req <- data.frame(
  Name = c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
  Type = c("Character", "Integer", "Integer", "Character", "Character", "Character", "Numeric", "Numeric", "Character", "Numeric", "Character", "Integer", "Numeric", "Numeric", "Numeric")
)

proton_mass <- 1.00727647

deuteration_mass <- 2.0141 - 1.008

dt_format <- function(dat, cols = colnames(dat)) {
  
  datatable(data = dat,
            colnames = cols,
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10, dom = "tBip", autoWidth = TRUE, buttons = c("excel", "pdf")),
            filter = "bottom",
            rownames = FALSE)
  
}

round_any <- function(x, accuracy, f = round){f(x/accuracy) * accuracy}

confidence_limit_choices <- c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999)

amino_groups <- c("G", "A", "V", "I", "L", "F", "P", "M", "S", "T", "Y", "W", "N", "Q", "C", "D", "E", "K", "R", "H")

plotOutput_h <- function(outputId, ...) 
  helper(withSpinner(plotOutput(outputId = outputId, ...)),  content = outputId,
         type = "markdown", buttonLabel = "Okay", easyClose = TRUE)

func_vec <- c("selectInput", "textInput", "checkboxInput", "numericInput")
func_list <- setNames(lapply(func_vec, function(ith_fun) 
  tmp_name <- function(inputId, ...) {
    helper(getFromNamespace(ith_fun, ns = "shiny")(inputId = inputId, ...),  
           content = inputId, type = "markdown", buttonLabel = "Okay", easyClose = TRUE)
  }), func_vec)

for(ith_fun_id in 1L:length(func_list)) {
  assign(x = paste0(names(func_list)[ith_fun_id], "_h"), value = func_list[[ith_fun_id]])
}


# get("selectInput", envir = "package::shiny")
