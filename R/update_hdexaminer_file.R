#' Update data from HDeXaminer
#' 
#' @description Updates data read from file with detected data source HDeXaminer 
#' 
#' @importFrom data.table data.table
#' 
#' @param dat a data frame with data read from file by \code{\link{read_hdx}}
#' @param fd_time time point (in minutes) for fully deuterated sample. Numeric value, bigger than other
#' time points in the file. Obligatory
#' @param old_protein_name protein name to be changed. String or string vector (if the file contains
#' data from multiple proteins). Not obligatory
#' @param new_protein_name new names for old_protein_name. String or string vector (if the file contains
#' data from multiple proteins). Not obligatory
#' @param old_state_name state names to be changed. String or string vector (if the file contains
#' data from multiple states). Not obligatory
#' @param new_state_name new names for old_state_name. String or string vector (if the file contains
#' data from multiple states). Not obligatory
#' @param confidence vector of accepted confidence values (internal flag from HDeXaminer). By default only
#' accepted values are `Medium` and `High`, with `Low` excluded. The user can control that by specifying 
#' this parameter
#' 
#' @details Data from HDeXaminer is condensed and automated data retrieving may be corrected by the user. 
#' The original file has a mark "FD" for fully deuterated data instead of numerical value for time point 
#' (provided in minutes) that is not consistent for workflow and not enough for precise data description.
#' Moreover, the data about both protein and state is included in one column and for detailed information
#' function \code{\link{update_hdexaminer_file}} allows to change them.
#' 
#' @return \code{dat} - a \code{\link{data.frame}} with validated and updated content.
#' 
#' @seealso \code{\link{read_hdx}} \code{\link{calculate_kinetics}} \code{\link{calculate_state_deuteration}} 
#' \code{\link{plot_coverage}} \code{\link{plot_position_frequency}}
#' \code{\link{prepare_dataset}} \code{\link{quality_control}} \code{\link{reconstruct_sequence}}
#' 
#' @examples
#' # read example data 
#' dat_hdexaminer <- read_hdx(system.file(package = "HaDeX", 
#'                      "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#'                      
#' 
#' @export update_hdexaminer_file

update_hdexaminer_file <- function(dat,
                                   fd_time,
                                   old_protein_name = NULL,
                                   new_protein_name = NULL,
                                   old_state_name = NULL,  
                                   new_state_name = NULL,
                                   confidence = c("High", "Medium")){
  
  msg = ""
  
  if (attr(dat, "source") != "HDeXaminer"){
    stop("Supplied file is not of HDeXaminer origin.")
  }
  
  if(!is.numeric(fd_time)){
    stop("Supplied fd value is not numeric.")
  }
  
  dat <- data.table(dat) 
  
  if(fd_time < max(dat[Exposure!=99998, Exposure])){
    stop("Supplied fd value is smaller than time points from file.")
  }
  
  if(is.null(confidence)){
    stop("No confidence values provided.")
  }
  
  #validate if there is confidence column
  if(!("Confidence" %in% colnames(dat))){
    stop("No Confidence column. Check the data!")
  }
  
  if(!all(confidence %in% c("High", "Medium", "Low"))){
    stop("Not accetable condifdence values.")
  }
  
  dat <- dat[Confidence %in% confidence]
  
  dat <- dat[Exposure == 99998, `:=`(Exposure = fd_time)]
  msg <- paste0(msg, "FD value changed to ", fd_time, " min. ")
  
  if(length(old_protein_name) != length(new_protein_name)){
    stop("values size in old_protein_name and new_protein_name differ. ")
  }
  
  if(!is.null(old_protein_name) & !is.null(new_protein_name)){
    lapply(1:length(old_protein_name), function(i){
      dat[Protein == old_protein_name[i], `:=`(Protein = new_protein_name[i])]
    })
    msg <- paste0(msg, "Names ", old_protein_name, " replaced with ", new_protein_name, ". ")
  } else {
    msg <- paste0(msg, "Protein name not changed. ")
  }
  
  if(length(old_state_name) != length(new_state_name)){
    stop("values size in old_state_name and new_state_name differ. ")
  }
  
  if(!is.null(old_state_name) & !is.null(new_state_name)){
    lapply(1:length(old_state_name), function(i){
      dat[State == old_state_name[i], `:=`(State = new_state_name[i])]
    })
    msg <- paste0(msg, "Names ", old_state_name, " replaced with ", new_state_name, ". ")
  } else {
    msg <- paste0(msg, "State name not changed. ")
  }
  
  dat <- dat[, !"Confidence"]
  
  data.frame(dat)
  
  attr(dat, "source") <- "HDeXaminer"
  
  dat
  
}
