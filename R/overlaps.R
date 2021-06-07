#' Show data on overlap
#' 
#' @description Presents overlap data, based on the supplied
#' parameters.
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein.
#' @param state biological state for chosen protein.
#' @param start start start position of chosen protein.
#' @param end end position of chosen protein.
#' 
#' @details This data is available in the GUI. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\lnk{read_hdx}} 
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


#' Plot overlap data
#' 
#' @description Generates overapping peptide plot based 
#' on supplied data and parameters. 
#' 
#' @param dat produced by \code{\link{show_overlap_data}}
#' function
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{show_overlap_data}}
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
#' @importFrom dplyr right_join
#' @importFrom tidyr replace_na
#' @importFrom stringr str_length
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein.
#' @param state biological state for chosen protein.
#' @param start start start position of chosen protein.
#' @param end end position of chosen protein.
#' @param protein_sequence data produced by 
#' \code{\link{reconstruct_sequence}} function.
#' 
#' @details This data is available in the GUI.
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{reconstruct_sequence}} 
#'
#' @export create_overlap_distribution_dataset

create_overlap_distribution_dataset <- function(dat, 
                                                protein = dat[["Protein"]][1],
                                                state = dat[["State"]][1],
                                                start = min(dat[["Start"]]),
                                                end = max(dat[["End"]]),
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


#' Plot overlap distribution
#' 
#' @description Generates overlap distribution plot based on supplied data
#' and parameters.
#' 
#' @importFrom ggplot2 geom_hline geom_text
#' 
#' @param dat produced by \code{\link{create_overlap_distribution_dataset}}
#' function
#' @param start start start position of chosen protein.
#' @param end end position of chosen protein.
#' 
#' @details This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso 
#' \code{\link{{read_hdx}}
#' \code{\link{reconstruct_sequence}} 
#' \code{\link{create_overlap_distribution_dataset}}
#' 
#' @export plot_overlap_distribution

plot_overlap_distribution <- function(dat,
                                      start = 1,
                                      end = max(dat[["pos"]])){
  
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