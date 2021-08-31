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
#' @importFrom stringi stri_count
#' @importFrom dplyr group_by ungroup summarize
#'
#' @param filename a file supplied by the user. Formats allowed: .csv, .xlsx and .xls.
#'
#' @details Function \code{\link{read_hdx}} accepts files produced by DynamX 3.0 or 2.0
#' in `cluster data` format and `tables` file from HDeXaminer.
#' For further information check the documentation.
#' The function checks if all necessary columns are provided in correct format. The file must
#' include at least two repetitions of the measurement for the uncertainty to be calculated.
#' For the files of HDeXaminer origin, the rows with no complete information (e.q. missing
#' `Exp Cent` value) are removed. The `Confidence` column is preserved as the user should
#' have impact on accepting rows based on their Confidence flag. Moreover, those files need
#' action from the user - to confirm data processing (e.q. FD time point), choose accepted
#' confidence values and make some change of the labels use \code{\link{upadate_hdexaminer_file}}
#' function.
#' IMPORTANT! The files of HDeXaminer origin MUST be processed by hand or by
#' \code{\link{upadate_hdexaminer_file}} function to fit the input of processing functions
#' e.q. \code{\link{calculate_state_deuteration}} or \code{\link{calculate_kinetics}}.
#'
#' @return a \code{\link{data.frame}} object with validated content.
#'
#' @seealso
#' \code{\link{update_hdexaminer_file}}
#' \code{\link{create_control_dataset}}
#' \code{\link{calculate_state_deuteration}}
#'
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX",
#'                      "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' head(dat)
#'
#' @export read_hdx

read_hdx <- function(filename){

  dat <- switch(file_ext(filename),
                "csv" = fread(filename, data.table = FALSE),
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

  has_modification <- !all(is.na(dat[["Modification"]]))
  dat <- mutate(dat, State = paste0(State, ifelse(!is.na(Modification), paste0(" - ", Modification), ""))) %>%
    select(-Modification, -RT, -Fragment)

  dat[["Exposure"]] <- round(dat[["Exposure"]], 3)

  # attr(dat, "source") <- data_type
  # attr(dat, "has_modification") <- has_modification

  data.table(hdx_data(dat = dat,
                      source = data_type,
                      has_modification = has_modification))
  #
  # class(dat) <- c("hdx_data", "data.frame")
  #
  # dat


}

upgrade_2_to_3 <- function(dat){

  colnames(dat)[6] <- "MaxUptake"
  dat[["Fragment"]] <- NA

  dat

}

transform_examiner <- function(dat){

  # rows with missing data deleted
  dat <- dat[!is.na(`Exp Cent`)]
  # choose only useful columns
  dat <- dat[, c("Protein State", "Deut Time", "Experiment", "Start", "End", "Sequence", "Charge", "Search RT", "Max Inty", "Exp Cent", "Confidence")]
  # change names
  colnames(dat) <- c("State", "Exposure", "File", "Start", "End", "Sequence", "z", "RT", "Inten", "Center", "Confidence")
  # prepare Protein  column
  dat[, "Protein"] <- dat[order(nchar(State)), State][[1]]
  # change time from second to minutes
  dat[Exposure == "FD", `:=`(Exposure = "5999880")] # flag for fully deuterated sample # 99998
  dat[, `:=`(Exposure = round(parse_number(Exposure)/60, 4))]
  # in time for better precision
  dat[Exposure > 0 & Exposure < 0.001, `:=`(Exposure = 0.001)]
  #calculate MaxUptake
  dat[, `:=`(MaxUptake = nchar(Sequence) - 2 - stri_count(Sequence, fixed = "P"))]
  # calculate MPH
  dat[, `:=`(MHP = calculate_MHP(Sequence, mono = FALSE))]
  # columns to fit the required format
  dat[, `:=`(Fragment = NA,
             Modification  = NA)]

  dat
}
