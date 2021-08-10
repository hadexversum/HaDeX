#' Constructor of hdx_data class
#' 
#' @param dat \code{\link{data.frame}}, read from the datafile.
#' @param source \code{\link{character}}, the source of the datafile.
#' @param has_modification \code{\link{logical}}, indicator if there are 
#' modified peptides in the datafile.
#' 
#' @description Class hdx_data is the base of any calculation done in HaDeX. 
#' It structulizes the data from the data file read by the \code{\link{read_hdx}} 
#' function. The obect preserves the information of the data file origin - for now,
#' the function accepts datafiles from DynamX2.0, DynamX3.0 and HDeXaminer. The data 
#' from the datafile is checked and put in one format suitable for the package
#' workflow, regardless of its origin. 
#' 
#' The structure is as follows:
#' - Protein, character.
#' - Start, integer.
#' - End, integer.
#' - Sequence, character.
#' - MaxUptake, numeric.
#' - MHP, numeric.
#' - State, character.
#' - Exposure, numeric.
#' - File, character.
#' - z, integer.
#' - Inten, numeric. 
#' - Center, numeric.
#' 
#' The hdx_data class inherits from data.frame class, so the structure is preserved.
#' The hdx_data object has two additional attributes:
#' - source, character. Indicates the source of the datafile.
#' - has_modification, logical. Indicates if the datafile has data from modified
#' peptides.
#' 
#' 
#' @return \code{\link{hdx_data}} object.
#' 
#' @keyword Internal

new_hdx_data <- function(dat, source, has_modification){
  
  structure(dat,
            class = c("hdx_data", "data.frame"),
            source = source,
            has_modification = has_modification)
  
  
}

#' Validator on hdx_data class
#' 
#' @description Validator on the content of an hdx_data object.
#' 
#' @param hdx_data \code{\link{hdx_data}} object.
#' 
#' @keyword Internal

validate_hdx_data <- function(hdx_data, msg = ""){
  
  no_replicates <- hdx_data %>%
    group_by(Protein, Start, End, Sequence, State, Exposure) %>%
    summarise(n_rep = length(unique(File))) %>%
    ungroup(.) %>%
    summarize(avg_rep = mean(n_rep))
  
  if (!(no_replicates[[1]] >= 2)) {
    msg <- paste0(msg, "There is no sufficient number of replicates.")
  } 
  
  print(msg)
  hdx_data
}

#' Creation of validated hdx_data class
#' 
#' @inheritParams new_hdx_data
#' 
#' @description The wrapper function for the constructor of the \code{\link{hdx_class}} and 
#' its validator. Used in \code{\link{read_hdx}} function.
#' 
#' @return \code{\link{hdx_data}} object.
#' 
#' @keyword Internal

hdx_data <- function(dat, source, has_modification, msg = ""){
  
  tmp <- new_hdx_data(dat = dat,
                      source = source,
                      has_modification = has_modification)
  
  validate_hdx_data(tmp, msg = msg)
  
  tmp
  
}


