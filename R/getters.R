#' Get protein coverage
#' 
#' @description Calculate protein coverage by the peptides in 
#' selected biological state or states.
#' 
#' @importFrom stringi stri_count_fixed
#' @importFrom checkmate assert
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein_length \code{\link{numeric}}, indicates the length of 
#' the protein. If not provided, the maximal end value from the file is used.
#' 
#' @details Function \code{\link{get_protein_coverage}} calculates the 
#' percentage coverage of the protein sequence, rounded to two decimal places.
#'
#' @return a \code{\link{numeric}} percentage value (rounded to two decimal places).
#'
#' @seealso
#' \code{\link{read_hdx}}
#'
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' get_protein_coverage(dat)
#' get_protein_coverage(dat, protein_length = 150)
#' 
#' @export get_protein_coverage

get_protein_coverage <- function(dat,
                                 protein = dat[["Protein"]][1],
                                 states = unique(dat[["State"]]),
                                 protein_length = NULL){
  
  if (is.null(protein_length)){
    protein_length <- max(dat[["End"]])
  } 
  
  assert(protein_length>=max(dat[["End"]]))
  
  round(100*(protein_length - stri_count_fixed(reconstruct_sequence(dat, 
                                                                    end = protein_length, 
                                                                    protein = protein, 
                                                                    states = states), 'x'))/protein_length, 2)
  
}

#' Get protein redundancy
#' 
#' @description Calculates the protein redundancy in the whole 
#' experiment (all biological states).
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein_length \code{\link{numeric}}, indicates the length of 
#' the protein. If not provided, the maximal end value from the file is used.
#' 
#' @details Function \code{\link{get_protein_redundancy}} calculates the redundancy
#' of the protein, based on provided experimental data.
#' 
#' @return a \code{\link{numeric}} value.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' get_protein_redundancy(dat)
#' 
#' @export get_protein_redundancy

get_protein_redundancy <- function(dat, 
                                   protein_length = NULL){
  
  if(is.null(protein_length)){
    protein_length <- max(dat[["End"]])
  } else {
    assert(protein_length>=max(dat[["End"]]))
  }
  
  round(mean(create_overlap_distribution_dataset(dat, end = protein_length)[["coverage"]]), 2)
  
}

#' Get number of replicates
#' 
#' @description Calculates the number of replicates from 
#' the experimnetal data.
#' 
#' @importFrom dplyr pull
#' @importFrom data.table uniqueN
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' 
#' @details Calculate the number of replicates of experiment.
#' 
#' @return a \code{\link{numeric}} value.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' get_n_replicates(dat)
#'
#' @export get_n_replicates

get_n_replicates <- function(dat,
                             protein = dat[["Protein"]][1]){
  
  dat <- as.data.table(dat)
  
  dat <- dat[Protein == protein]
  dat <- dat[, .(n_rep = uniqueN(File)),
             by = c("Protein", "Start", "End", "Sequence", "State", "Exposure")]
  as.numeric(names(sort(table(dat[["n_rep"]]), decreasing = TRUE)))[1]
  
}

#' Get peptide sequence based on the position
#' 
#' @description Gets the peptide sequence based on selected parameters
#' (start and end position, modification).
#' 
#' @param dat any data frame that contains following information:
#' protein, sequence, start, end, modification.
#' @param protein chosen protein. 
#' @param start start position of the peptide of interest.
#' @param end end position of the peptide of interest.
#' @param modification logical value to indicate if peptide
#' of interest has modification or not.
#' 
#' @details Function returns peptide sequence for selected parameters. 
#' Peptide sequence is often required to properly identify peptide of 
#' interest, and to avoid mistakes sequence is returned by the function.
#' Moreover, function uses the modification value to select petide sequence.
#' 
#' @return a \code{\link{character}} value.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' get_peptide_sequence(dat, start = 1, end = 15)
#'
#' @export get_peptide_sequence

get_peptide_sequence <- function(dat, 
                                 protein = dat[["Protein"]][1],
                                 start, 
                                 end,
                                 modification = FALSE){
  
  sequences <- unique(dat[Protein == protein & Start == start & End == end, .(Sequence, Modification)])
  sequences[Modification!="", Sequence := paste0(Sequence, "+", Modification)]
  
  if(modification) { sequences[Modification!="" ][["Sequence"]] }  
  else { sequences[Modification=="" | is.na(Modification)][["Sequence"]] }
  
}