#' Read HDX-MS data file
#' 
#' @description Imports data from a HDX-MS file and validates its content.
#' 
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom readr read_csv read_tsv parse_logical parse_integer parse_double parse_character 
#' cols col_character parse_number
#' @importFrom data.table fread setattr `:=`
#' @importFrom dplyr %>%
#' @importFrom Peptides mw
#' @importFrom stringr str_count
#' 
#' @param filename a file supplied by the user. Formats allowed: .csv, .xlsx and .xls.
#' 
#' @details First version accepts files produced by DynamX 3.0 and 2.0 in `cluster data` format. 
#' The function checks if all necessary columns are provided in correct format. The file must 
#' include at least two repetitions of the measurement for the uncertainty to be calculated.
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
                "xlsx" = read_excel(filename),
                "xls" = read_excel(filename))
  
  data_type <- "Dynamx3.0"
  
  #check for hdexaminer file
  colnames_exam <- c("Protein State",  "Deut Time", "Experiment", 
                     "Start", "End", "Sequence", "Charge", "Search RT",
                     "Actual RT", "# Spectra", "Peak Width", "m/z Shift",
                     "Max Inty", "Exp Cent", "Theor Cent", "Score", "Cent Diff", 
                     "# Deut", "Deut %", "Confidence")
  
  if(all(colnames_exam %in% colnames(dat))){
    dat <- transform_examiner(dat)
    data_type <- "HDeXaminer"
  }
  
  #check for dynamx2 file
  colnames_v_2 <- c("Protein", "Start", "End", "Sequence", 
                    "Modification", "Max Exchangers", 
                    "MHP", "State", "Exposure", "File", 
                    "z", "RT", "Inten", "Center")
  
  if(all(colnames_v_2 %in% colnames(dat))){
    dat <- upgrade_2_to_3(dat)
    data_type <- "Dynamx2.0"
  }
  
  #check for dynamx3 file
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
  
  no_replicates <- dat %>%
    group_by(Protein, Start, End, Sequence, Modification, State, Exposure) %>%
    summarise(n_rep = length(unique(File))) %>%
    ungroup(.) %>%
    summarize(avg_rep = mean(n_rep))
    
  if (!(no_replicates[[1]] > 2)) {
    err_message <- "There is no sufficient number of replicates."
  } 
  
  dat[["Exposure"]] <- round(dat[["Exposure"]], 3)
  
  dat <- mutate(dat, State = paste0(State, ifelse(!is.na(Modification), paste0(" - ", Modification), "")))
  
  attr(dat, "source") <- data_type
  
  dat

  
}

upgrade_2_to_3 <- function(dat){
  
  colnames(dat)[6] <- "MaxUptake"
  dat[["Fragment"]] <- NA
  
  dat
  
}

transform_examiner <- function(dat){
 
  # low confidence rows deleted
  dat <- dat[Confidence != "Low"]
  # choose only useful columns
  dat <- dat[, c("Protein State", "Deut Time", "Experiment", "Start", "End", "Sequence", "Charge", "Search RT", "Max Inty", "Exp Cent")] 
  # change names
  colnames(dat) <- c("State", "Exposure", "File", "Start", "End", "Sequence", "z", "RT", "Inten", "Center")
  # prepare Protein  column
  dat[, "Protein"] <- dat[order(nchar(State)), State][[1]]
  # change time from second to minutes
  dat[Exposure == "FD", `:=`(Exposure = "5999880")] # flag for fully deuterated sample
  dat[, `:=`(Exposure = parse_number(Exposure)/60)]
  #calculate MaxUptake
  dat[, `:=`(MaxUptake = nchar(Sequence) - 2 - str_count(Sequence, "P"))]
  # calculate MPH
  dat[, `:=`(MHP = mw(Sequence))]
  # columns to fit the required format
  dat[, `:=`(Fragment = NA,
             Modification  = NA)]
  
  dat 
}