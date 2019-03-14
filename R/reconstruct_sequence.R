#' reconstruct_sequence
#'
#' Reconstructs protein sequence from DynamiX file. If a position is not covered by experiment data, x is shown.
#'
#' @importFrom dplyr %>% bind_rows
#' 
#' @param dat data frame with data from Dynamix file
#' 
#' @return reconstructed sequence (character)
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' rec_seq <- reconstruct_sequence(dat)
#' 
#' @export reconstruct_sequence

reconstruct_sequence <- function(dat) {
  
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