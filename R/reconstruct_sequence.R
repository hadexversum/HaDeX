#' Reconstruct protein sequence
#'
#' @description Reconstruct protein sequence from
#' experimental data
#' 
#' @importFrom dplyr %>% bind_rows
#' 
#' @param dat data read by \code{\link{read_hdx}}
#' @param protein selected protein
#' @param states selected biological states for given protein
#' @param end \code{\link{numeric}}, end position of the protein
#' 
#' @details The function \code{\link{reconstruct_sequence}} 
#' generates protein sequence from supplied experimental data. 
#' For a position not covered, letter x is shown.
#' If the C-terminus of protein is not covered, there is a
#' possibility to manually fix the protein lenght by passing
#' a value to the `end` parameter.
#' 
#' @return a \code{\link{character}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
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