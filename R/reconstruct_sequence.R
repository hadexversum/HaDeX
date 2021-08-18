#' Reconstruct protein sequence
#'
#' @description Reconstructs protein sequence from supplied file. 
#'
#' @importFrom dplyr %>% bind_rows
#' 
#' @param dat data read by \code{\link{read_hdx}}
#' @param protein the protein of which the structure is to be reconstructed
#' @param end \code{\link{numerical}}, end position of the protein, optional.
#' If not provided, is read from the file. It allows to prelongate the sequence
#' if the end of the sequence is cut.
#' 
#' @details The function reconstructs protein sequence from supplied experimental data. If a position is not covered, x is shown.
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
                                 end = NULL) {

  if (is.null(end)) end <- max(dat[["End"]])
  
  position_in_sequence_tmp <- dat %>%
    filter(Protein == protein) %>%
    select(Start, End, Sequence) %>%
    unique(.) %>%
    apply(1, function(x) data.frame(position = x[1]:x[2], amino = strsplit(x[3], '')[[1]], stringsAsFactors = FALSE)) %>%
    bind_rows() %>%
    unique(.) 
  
  protein_sequence_template <- rep('x', end) 
  
  protein_sequence_template[position_in_sequence_tmp[["position"]]] <- position_in_sequence_tmp[["amino"]]
  
  paste(protein_sequence_template, collapse = "")
  
}