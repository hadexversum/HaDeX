#'generate_overlap_distribution_data
#' 
#' @description ...
#' 
#' @param dat ...
#' @param protein ...
#' @param state ...
#' @param start ...
#' @param end ...
#' @param protein_sequence ...
#' 
#' @details ...
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_overlap_distribution_data

generate_overlap_distribution_data <- function(dat, 
                                               protein,
                                               state,
                                               start,
                                               end,
                                               protein_sequence){
  
  dat %>%
    select(Protein, Start, End, State, Sequence) %>%
    filter(Protein == protein) %>%
    filter(State == state) %>%
    filter(Start >= start, End <= end) %>%
    filter(!duplicated(.)) %>%
    select(-State, -Protein) %>%
    apply(1, function(i) i[1]:i[2]) %>%
    unlist %>%
    data.frame(pos = .) %>%
    group_by(pos) %>%
    summarise(coverage = length(pos)) %>%
    right_join(data.frame(pos = seq(from = start, to = end))) %>%
    replace_na(list(coverage = 0)) %>%
    right_join(data.frame(amino = unlist(strsplit(protein_sequence, "")), 
                          pos = 1:str_length(protein_sequence))) %>%
    select(pos, amino, coverage)
  
  
}