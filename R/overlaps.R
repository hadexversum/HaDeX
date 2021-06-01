#' show_overlap_data
#' 
#' @description Generates overlap data, based on the supplied
#' parameters.
#' 
#' @param dat ...
#' @param protein ...
#' @param state ...
#' @param start ...
#' @param end ...
#' 
#' @details This data is available in the GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export show_overlap_data

show_overlap_data <- function(dat,
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


#' generate_overlap_plot
#' 
#' @description Generates overapping peptide plot based on supplied data
#' and parameters. 
#' 
#' @param dat produced by \code{\link{generate_overlap_plot}}
#' function
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export plot_overlap

plot_overlap <- function(dat){
  
  dat %>%
    select(Sequence, Start, End) %>%
    filter(!duplicated(.)) %>%
    arrange(Start, End) %>%
    mutate(ID = row_number()) %>%
    ggplot() +
    geom_segment(aes(x = Start, y = ID, xend = End, yend = ID)) +
    labs(title = "Peptide coverage",
         x = "Position",
         y = "") +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) 
}


#' generate_overlap_distribution_data
#' 
#' @description Generates the data set of frequency of overlap of
#' each amino in the protein sequence.
#' 
#' @param dat ...
#' @param protein ...
#' @param state ...
#' @param start ...
#' @param end ...
#' @param protein_sequence ...
#' 
#' @details This data is available in the GUI.
#' 
#' @return ...
#' 
#' @seealso ... 
# '
#' @export create_overlap_distribution_dataset

create_overlap_distribution_dataset <- function(dat, 
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


#' generate_overlap_distribution_plot
#' 
#' @description Generates overlap distribution plot based on supplied data
#' and parameters.
#' 
#' @param dat produced by \code{\link{generate_overlap_distribution_plot}}
#' function
#' @param start ...
#' @param end ...
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return ...
#' 
#' @seealso ... 
#' 
#' @export plot_overlap_distribution

plot_overlap_distribution <- function(dat,
                                               start,
                                               end){
  
  mean_coverage <- round(mean(dat[["coverage"]], na.rm = TRUE), 2)
  display_position <- (start + end)/2
  
  dat %>% 
    ggplot(aes(x = pos, y = coverage)) +
    geom_col(width = 1) +
    labs(x = 'Position', y = 'Position frequency in peptides') +
    theme(legend.position = "none") + 
    coord_cartesian(xlim = c(start, end)) +
    geom_hline(yintercept = mean_coverage, color = 'red') +
    geom_text(aes(x = display_position, y = mean_coverage), label = paste0("Average frequency: ", mean_coverage), color = 'red', vjust = -.5)
  
}