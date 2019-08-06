#' Read HDX-MS data file
#' 
#' @description Imports data from a HDX-MS file and validates its content.
#' 
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom readr read_csv read_tsv parse_logical parse_integer parse_double parse_character 
#' cols col_character
#' @importFrom data.table fread
#' 
#' @param filename a file supplied by the user. Formats allowed: .csv, .xlsx and .xls.
#' 
#' @details First version accepts files produced by DynamX 3.0 and 2.0 in basic format. 
#' The function checks if all necessary columns are provided in correct format.
#' 
#' @return \code{dat} - a \code{\link{data.frame}} with validated content.
#' 
#' @seealso \code{\link{calculate_kinetics}} \code{\link{calculate_state_deuteration}} \code{\link{plot_coverage}} \code{\link{plot_position_frequency}}
#' \code{\link{prepare_dataset}} \code{\link{quality_control}} \code{\link{reconstruct_sequence}}
#' 
#' @examples
#' # read example data
#' head(read_hdx(system.file(package = "HaDeX", 
#'                      "HaDeX/data/KD_180110_CD160_HVEM.csv")))
#' 
#' @export read_hdx

read_hdx <- function(filename){
  
  dat <- switch(file_ext(filename),
                "csv" = fread(filename),
                # "csv" = read_csv(filename, col_names = TRUE, col_types = cols(Modification = col_character(), 
                #                                                               Fragment = col_character())),
                # "tsv" = read_tsv(filename, col_names = TRUE, col_types = cols(Modification = col_character(), 
                #                                                               Fragment = col_character())),
                "xlsx" = read_excel(filename),
                "xls" = read_excel(filename))
  
  #check for dynamx2 file
  colnames_v_2 <- c("Protein", "Start", "End", "Sequence", 
                    "Modification", "Max Exchangers", 
                    "MHP", "State", "Exposure", "File", 
                    "z", "RT", "Inten", "Center")
  
  if(all(colnames_v_2 %in% colnames(dat))){
    dat <- upgrade_2_to_3(dat)
  }
  
  colnames_v_3 <- c("Protein", "Start", "End", "Sequence", 
                  "Modification", "Fragment", "MaxUptake", 
                  "MHP", "State", "Exposure", "File", "z", 
                  "RT", "Inten", "Center") 
  
  colnames_presence <- colnames_v_3 %in% colnames(dat)
  
  if(!all(colnames_presence)) {
    err_message <- paste0(ifelse(sum(!colnames_presence) > 0, 
                                 "A supplied file does not have required columns: ", 
                                 "A supplied file does not have the required column "),
                          paste0(colnames_v_3[!colnames_presence], collapse = ", "), ".")
    stop(err_message)
  }
  
  
  dat[["Exposure"]] <- round(dat[["Exposure"]], 3)
  
  dat <- mutate(dat, State = paste0(State, ifelse(!is.na(Modification), paste0(" - ", Modification), ""))) 
  
  dat
  
}

upgrade_2_to_3 <- function(dat){
  
  colnames(dat)[6] <- "MaxUptake"
  dat[["Fragment"]] <- NA
  
  dat
  
}
