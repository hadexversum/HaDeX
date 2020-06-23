#' generate_overlap_data
#' 
#' @description ...
#' 
#' @param dat ...
#' @param protein ...
#' @param state ...
#' @param start ...
#' @param end ...
#' 
#' @details ...
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_overlap_data

generate_overlap_data <- function(dat,
                                  protein,
                                  state,
                                  start,
                                  end){
  dat %>%
    select(Protein, Sequence, Start, End, State) %>% 
    filter(Protein == protein) %>%
    filter(State == state) %>%
    filter(Start >= start, End <= end) %>%
    filter(!duplicated(.)) %>%
    select(-State)
}