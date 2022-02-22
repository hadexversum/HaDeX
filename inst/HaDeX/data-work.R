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
library(shinycustomloader)
library(shinyhelper)
library(shinyjs)
library(gridExtra)

#########################################

HaDeX_theme <- theme_bw() + theme(plot.background = element_rect(fill = NA, color = NA))

theme_set(HaDeX_theme) 

amino_prop <- read.csv("./data/amino_prop.csv")

amino_prop <- amino_prop %>%
  mutate(charge = factor(charge, levels = c("-1", "0", "1")))

file_req <- data.frame(
  Name = c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
  Type = c("Character", "Integer", "Integer", "Character", "Character", "Character", "Numeric", "Numeric", "Character", "Numeric", "Character", "Integer", "Numeric", "Numeric", "Numeric"),
  Obligatory = c("TRUE", "TRUE", "TRUE", "TRUE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE"),
  Description = c("Protein name", "Peptide's first amino acid position in sequence", "Peptide's last amino acid position in sequence", "Peptide's sequence in single amino acid in one letter code", "Post-transitional modification of the peptyde", "Fragment label from ETD-HDX data", "Number od maximal deuteriul atoms uptake", "mass of the singly charged molecular ion", "Name od the protein state", "D2O exposure in minutes", "Name of .raw file", "Charge", "Peptide's retention time in minures", "Intensity", "Measured mass to charge ratio"),
  DynamX3.0 = c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
  DynamX2.0 = c("Protein", "Start", "End", "Sequence", "No information", "No information", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center"),
  HDeXaminer = c("Shortest value from `Protein State`", "Start", "End", "Sequence", "No information", "No information", "Calculated based on sequence", "Calculated based on sequence", "Protein State", "Deut Time", "Experiment", "Charge", "Search RT", "Max Inty", "Exp Cent")
  
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

confidence_level_choices <- c("20%" = 0.2, "50%" = 0.5, "80%" = 0.8, "90%" = 0.9, "95%" = 0.95, "98%" = 0.98, "99%" = 0.99, "99.9%" = 0.999)

amino_groups <- c("G", "A", "V", "I", "L", "F", "P", "M", "S", "T", "Y", "W", "N", "Q", "C", "D", "E", "K", "R", "H")

withHaDeXSpinner <- function(ui_element) withLoader(ui_element, type = "image", loader = "HaDeX_loader.gif")

plotOutput_h <- function(outputId, ...) 
  helper(withHaDeXSpinner(plotOutput(outputId = outputId, ...)),  
         content = outputId, type = "markdown", buttonLabel = "Okay", easyClose = TRUE, 
         icon = "far fa-question-circle", colour = "#856C9D")

## "DT::dataTableOutput"
dataTableOutput_h <- function(outputId, ...)
  helper(getFromNamespace("dataTableOutput", ns = "DT")(outputId = outputId, ...), content = outputId, 
         type = "markdown", buttonLabel = "okay", easyClose = TRUE, 
         icon = "far fa-question-circle", colour = "#856C9D")

func_vec <- c("selectInput", "textInput", "checkboxInput", "numericInput", "radioButtons", "checkboxGroupInput")
func_list <- setNames(lapply(func_vec, function(ith_fun) 
  tmp_name <- function(inputId, ...) {
    helper(getFromNamespace(ith_fun, ns = "shiny")(inputId = inputId, ...),  content = inputId, 
           type = "markdown", buttonLabel = "Okay", easyClose = TRUE, 
           icon = "far fa-question-circle", colour = "#856C9D")
  }), func_vec)

for(ith_fun_id in 1L:length(func_list)) {
  assign(x = paste0(names(func_list)[ith_fun_id], "_h"), value = func_list[[ith_fun_id]])
}

  
# get("selectInput", envir = "package::shiny")

get_internal_messages <- function(expr){
  
  tryCatch({
    expr
  },
  error = function(e){
    validate(need(FALSE, conditionMessage(e)))
  })
  
}