#' Read HDX-MS data file
#' 
#' Imports data from a HDX-MS file (as provided by Waters DynamX software) 
#' and validates its content.
#' 
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom readr read_csv read_tsv parse_logical parse_integer parse_double parse_character
#' 
#' @param filename a file supplied by a user. Formats allowed: .csv, .tsv and .xls.
#' 
#' @return a \code{data.frame}.
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", 
#'                             "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' 
#' @export read_hdx

read_hdx <- function(filename){
  
  dat <- switch(file_ext(filename),
                "csv" = read_csv(filename, col_names = TRUE),
                "tsv" = read_tsv(filename, col_names = TRUE),
                "xls" = read_excel(filename))
  
  colnames_v <- c("Protein", "Start", "End", "Sequence", 
                  "Modification", "Fragment", "MaxUptake", 
                  "MHP", "State", "Exposure", "File", "z", 
                  "RT", "Inten", "Center") 
  
  colnames_presence <- colnames_v %in% colnames(dat)
  
  if(!all(colnames_presence)) {
    err_message <- paste0(ifelse(sum(!colnames_presence) > 0, 
                                 "A supplied file does not have required columns: ", 
                                 "A supplied file does not have the required column "),
                          paste0(colnames_v[!colnames_presence], collapse = ", "), ".")
    stop(err_message)
  }
  
  
  dat[["Exposure"]] <- round(dat[["Exposure"]], 3)
  
  dat
  
}
