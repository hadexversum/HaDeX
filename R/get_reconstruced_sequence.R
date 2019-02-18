#' get_reconstruced_sequence
#'
#' Reconstructs sequence from DynamiX file. If a position is not covered by experiment, x is shown.
#'
#' @export get_reconstructed_sequence
#' @import dplyr

get_reconstructed_sequence <- function(dat) {
  
  position_in_sequence_tmp <- dat %>%
    select(Start, End, Sequence) %>%
    unique(.) %>%
    apply(1, function(x) data.frame(position = x[1]:x[2], amino = strsplit(x[3], '')[[1]], stringsAsFactors = FALSE)) %>%
    bind_rows() %>%
    unique(.) 
  
  protein_sequence_template <- rep('x', max(dat[["End"]])) 
  
  protein_sequence_template[position_in_sequence_tmp[["position"]]] <- position_in_sequence_tmp[["amino"]]
  
  paste(protein_sequence_template, collapse = "")
  
}