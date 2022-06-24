#' Reconstruct protein sequence
#'
#' @description Reconstructs protein sequence from supplied file. 
#'
#' @importFrom dplyr %>% bind_rows
#' 
#' @param dat data read by \code{\link{read_hdx}}
#' @param protein the protein of which the sequence is reconstructed
#' @param states biological states, for which the sequence is reconstructed
#' and coverage calculated
#' @param end \code{\link{numeric}}, end position of the protein, optional.
#' If not provided, is read from the file. It allows to prelongate the sequence
#' if the end of the sequence is cut.
#' 
#' @details The function reconstructs protein sequence from supplied 
#' experimental data. If a position is not covered, x is shown.
#' First version doesn't support manual sequence length correction.
#' 
#' @return reconstructed sequence - \code{character} object.
#' 
#' @seealso \code{\link{read_hdx}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' reconstruct_sequence(dat)
#' 
#' @export reconstruct_sequence

reconstruct_sequence <- function(dat, 
                                 protein = dat[["Protein"]][1],
                                 states = unique(dat[["State"]]),
                                 end = NULL) {
  dat <- data.table(dat)
  
  if (is.null(end)) end <- max(dat[["End"]])
  
  tmp_dat <- dat[Protein == protein]
  tmp_dat <- unique(tmp_dat[,.(Start, End, Sequence)])
  
  position_in_sequence_tmp <- unique(rbindlist(apply(tmp_dat, 1, function(x) data.table(position = x[1]:x[2],
                                                                                        amino = strsplit(x[3], '')[[1]],
                                                                                        stringsAsFactors = FALSE))))
  protein_sequence_template <- rep('x', end)
  
  protein_sequence_template[position_in_sequence_tmp[["position"]]] <- position_in_sequence_tmp[["amino"]]
  
  paste(protein_sequence_template, collapse = "")
  
  
}