#' generate_general_data_set
#' 
#' @description Generates the data set with control sample, based on 
#' supplied parameters.
#' 
#' @param dat ...
#' @param control_protein ...
#' @param control_state ...
#' @param control_exposure ...
#' 
#' @details The names of the parameters and variables will be changed 
#' later after the glossary project.
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_general_data_set

generate_general_data_set <- function(dat,
                                      control_protein,
                                      control_state,
                                      control_exposure){
  
  tmp <- dat %>%
    filter(Protein == control_protein, 
           State == control_state, 
           Exposure == control_exposure) %>%
    mutate(Exposure = 99999)
  
  states_to_prepare <- unique(filter(dat, Protein == control_protein)[["State"]])
  
  bind_rows(dat, 
            lapply(states_to_prepare, function(state){
              peps <- dat %>%
                filter(State == state) %>%
                select(Sequence) %>%
                unique(.) %>%
                unlist(.)
              tmp %>%
                filter(Sequence %in% peps) %>%
                mutate(State = state) 
            }))
  
}