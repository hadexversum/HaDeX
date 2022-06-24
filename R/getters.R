#' Get protein coverage
#'
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein_length \code{\link{numeric}}, indicates the length of 
#' the protein. If not provided, the maximal end value from the file is used.
#' 
#' @importFrom stringi stri_count_fixed
#' @importFrom checkmate assert
#' 
#' @details Function \code{\link{get_protein_coverage}} calculates the percentage coverage of the 
#' protein sequence, rounded to two decimal places
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
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' 
#' @importFrom dplyr pull
#' @importFrom data.table uniqueN
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
  
  dat <- data.table(dat)

  dat <- dat[Protein == protein]
  dat <- dat[, .(n_rep = uniqueN(File)),
             by = c("Protein", "Start", "End", "Sequence", "State", "Exposure")]
  as.numeric(names(sort(table(dat[["n_rep"]]), decreasing = TRUE)))[1]
  
}