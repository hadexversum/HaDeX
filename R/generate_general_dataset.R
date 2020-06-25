#' generate_general_dataset
#' 
#' @description Generates the dataset with control sample 
#' based on supplied parameters.
#' 
#' @param dat ...
#' @param control_protein ...
#' @param control_state ...
#' @param control_exposure ...
#' 
#' @details ...
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export generate_general_dataset

generate_general_dataset <- function(dat,
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