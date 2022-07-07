#' generate_amino_distribution
#' 
#' @description Generates amino distribution based on the 
#' protein sequence and shows if the amino acid is hydrophobic
#' or hydrophylic.
#' 
#' @param position_in_sequence custom format
#' @param hydro_properties ...
#' @param protein ...
#' @param charge_colors ...
#' 
#' @details The data for this function is not packaged yet.
#' 
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso ...
#' 
#' @examples ...
#' 
#' @export plot_amino_distribution

plot_amino_distribution <- function(position_in_sequence, 
                                    hydro_properties,
                                    protein,
                                    charge_colors){
  
  position_in_sequence %>%
    mutate(affinity = ifelse(is_hydrophobic, "phobic", "philic")) %>% 
    filter(affinity %in% hydro_properties) %>%
    mutate(amino = factor(amino, levels = amino_groups)) %>%
    group_by(amino, charge, is_hydrophobic) %>%
    summarise(cnt = n()) %>%
    ggplot(aes(x = amino, y = cnt, fill = charge)) + 
    geom_col() +
    scale_fill_manual("Charge", values = charge_colors) + 
    labs(title = paste0('Amino acid composition for ', protein),
         x = 'Amino acid',
         y = 'Count')
  
}