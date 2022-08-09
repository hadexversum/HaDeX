#' Show data on peptide overlap
#' 
#' @description Presents peptide overlap on protein sequence data, 
#' based on the supplied parameters. 
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein.
#' @param state biological state for chosen protein.
#' @param start start position of chosen protein.
#' @param end end position of chosen protein.
#' 
#' @details The data frame presents all the peptides in given state, with
#' its start and end position on the protein sequence.
#' This data is available in the GUI. 
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}} 
#' \code{\link{plot_overlap}}
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' show_overlap_data(dat)
#' 
#' @export show_overlap_data

show_overlap_data <- function(dat,
                              protein = dat[["Protein"]][1],
                              state = dat[["State"]][1],
                              start = min(dat[["Start"]]),
                              end = max(dat[["End"]])){
  dat <- data.table(dat)
  
  dat <- dat[Protein == protein & State == state & Start >= start & End <= end,
             .(Protein, Sequence, Start, End, State)]
  dat <- dat[!duplicated(dat)]
  setorderv(dat, cols = c("Start", "End"))
  dat[, ID := 1:.N]
  dat[["State"]] <- NULL
  
  setcolorder(dat, c("Protein", "Sequence", "ID", "Start", "End"))
  
  dat
}


#' Plot overlap data
#' 
#' @description Generates overapping peptide plot based 
#' on supplied data and parameters. 
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein protein included in calculations
#' @param state state included in calculations
#' 
#' @details The overlap plot presents all the peptides in given state
#' on the protein sequence. 
#' This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{show_overlap_data}}
#' 
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' plot_overlap(dat)
#' 
#' @export plot_overlap

plot_overlap <- function(dat,
                         protein = dat[["Protein"]][1],
                         state = dat[["State"]][1]){
  
  dat <- dat[dat[["Protein"]] == protein & dat[["State"]] == state, ]
  
  dat <- data.table(dat)
  
  dat <- dat[, .(Sequence, Start, End)]
  dat <- dat[!duplicated(dat)]
  setorderv(dat, cols = c("Start", "End"))
  dat[, ID := 1:.N]
  
  overlap_plot <- ggplot(dat) +
    geom_segment(aes(x = Start, y = ID, xend = End, yend = ID)) +
    labs(title = "Peptide coverage",
         x = "Position",
         y = "") +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) 
  
  HaDeXify(overlap_plot)
  
}


#' Show overlap distribution data 
#' 
#' @description Generates the data of frequency of overlap of
#' each amino in the protein sequence.
#' 
#' @importFrom dplyr right_join
#' @importFrom tidyr replace_na
#' @importFrom stringr str_length
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein.
#' @param state biological state for chosen protein.
#' @param start start position of chosen protein.
#' @param end end position of chosen protein.
#' @param protein_sequence data produced by 
#' \code{\link{reconstruct_sequence}} function.
#' 
#' @details This data frame presents how many times (by how many peptides) 
#' a amino position in protein sequence is covered. 
#' This data is available in the GUI.
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{reconstruct_sequence}} 
#'
#' @examples
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' create_overlap_distribution_dataset(dat)
#' 
#' @export create_overlap_distribution_dataset

create_overlap_distribution_dataset <- function(dat, 
                                                protein = dat[["Protein"]][1],
                                                state = dat[["State"]][1],
                                                start = min(dat[["Start"]]),
                                                end = max(dat[["End"]]),
                                                protein_sequence = reconstruct_sequence(dat)){
  
  tmp_dat <- data.table(dat)
  
  tmp_dat <- tmp_dat[Protein == protein & State == state & Start >= start & End <= end,
             .(Protein, Start, End, State, Sequence)]
  tmp_dat <- tmp_dat[!duplicated(tmp_dat)]
  tmp_dat[, `:=`(State = NULL, Protein = NULL)]
  
  dt <- data.table(table(unlist(apply(tmp_dat, 1, function(i) i[1]:i[2]))))
  setnames(dt, c("V1", "N"), c("pos", "coverage"))
  dt[, pos := as.numeric(pos)]
  
  dt <- merge.data.table(dt, data.table(pos = start:end), all.y = TRUE)
  dt[is.na(dt)] <- 0
  
  tmp <- data.table(amino = unlist(tstrsplit(protein_sequence, "")),
                    pos = 1:nchar(protein_sequence))
  
  dt <- merge.data.table(dt, tmp, all.y = TRUE)[, .(pos, amino, coverage)]
  
  dt
  
}


#' Plot overlap distribution
#' 
#' @description Generates overlap distribution plot based on supplied data
#' and parameters.
#' 
#' @importFrom ggplot2 geom_hline geom_text
#' 
#' @param overlap_dist_dat produced by \code{\link{create_overlap_distribution_dataset}}
#' function
#' @param start start start position of chosen protein.
#' @param end end position of chosen protein.
#' @inheritParams plot_butterfly
#' 
#' @details This plot presents how many times (by how many peptides) 
#' a amino position in protein sequence is covered. 
#' This plot is visible in GUI. 
#' 
#' @return a \code{\link{ggplot2}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{reconstruct_sequence}} 
#' \code{\link{create_overlap_distribution_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' overlap_dist_dat <- create_overlap_distribution_dataset(dat)
#' plot_overlap_distribution(overlap_dist_dat)
#' 
#' @export plot_overlap_distribution

plot_overlap_distribution <- function(overlap_dist_dat,
                                      start = 1,
                                      end = max(overlap_dist_dat[["pos"]]),
                                      interactive = getOption("hadex_use_interactive_plots")){
  
  mean_coverage <- round(mean(overlap_dist_dat[["coverage"]], na.rm = TRUE), 2)
  display_position <- (start + end)/2
  
  chosen_geom_col <- if (interactive) ggiraph::geom_col_interactive(
    aes(tooltip = glue(
      "Position: {pos}
       Amino acid: {amino}
       Coverage: {coverage}"
    )),
    width = 1
  ) else geom_col(width = 1)
  
  overlap_dist_dat_plot <- ggplot(overlap_dist_dat, aes(x = pos, y = coverage)) +
    chosen_geom_col +
    labs(x = 'Position', y = 'Position frequency in peptides') +
    theme(legend.position = "none") + 
    coord_cartesian(xlim = c(start, end)) +
    geom_hline(yintercept = mean_coverage, color = 'red') +
    labs(caption = paste0("Average frequency ofshow range: ", mean_coverage))
  
  HaDeXify(overlap_dist_dat_plot)
  
}