#' read_hdx
#' 
#' Imports data from file and validates its content.
#' 
#' @importFrom readxl read_excel
#' @importFrom readr parse_logical parse_integer parse_double parse_character
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
  
  dat <- if (grepl(".csv", filename)){
    read.table(filename, sep = ",", header = TRUE, stringsAsFactors = FALSE)
  } else if (grepl(".tsv", filename)) {
    read.table(filename, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  } else if(grepl(".xls", filename)) {
    read_excel(filename)
  }
  
  tryCatch({
    
    dat <- mutate(dat,
                  Protein = parse_character(Protein),
                  Start = parse_integer(Start),
                  End = parse_integer(End),
                  Sequence = parse_character(Sequence),
                  # Modification = parse_character(Modification),
                  # Fragment = parse_character(Fragment),
                  MaxUptake = parse_double(MaxUptake),
                  MHP = parse_double(MHP),
                  State = parse_character(State),
                  Exposure = round(parse_double(Exposure),3),
                  File = parse_character(File),
                  z = parse_integer(z),
                  RT = parse_double(RT),
                  Inten = parse_double(Inten),
                  Center = parse_double(Center))
    
  }, warning = function(w){

    print("Check your file")

  }, error = function(e){

    print("Check your file!")

  }, finally = {

    dat
  
  })
  
}