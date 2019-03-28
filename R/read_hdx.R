#' read_hdx
#' 
#' Imports data from file and validates its content.
#' 
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom readr read_csv read_tsv parse_logical parse_integer parse_double parse_character
#' 
#' @param filename file supplied by user
#' 
#' @return data frame for further modifications
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' @export read_hdx

read_hdx <- function(filename){
  
  dat <- switch(file_ext(filename),
                "csv" = read_csv(filename, col_names = TRUE),
                "tsv" = read_tsv(filename, col_names = TRUE),
                "xls" = read_excel(filename))
  
  if (isTRUE(all.equal(colnames(dat), c("Protein", "Start", "End", "Sequence", "Modification", "Fragment", "MaxUptake", "MHP", "State", "Exposure", "File", "z", "RT", "Inten", "Center")))) {
    
    dat[["Exposure"]] <- round(dat[["Exposure"]], 3)
    
    dat
      
  } else {
    
    stop("Your file doesn\'t meet criteria.")
    
  }
  
}