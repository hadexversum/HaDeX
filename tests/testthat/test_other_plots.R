dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

chosen_protein <- "db_CD160"
chosen_state <- "CD160"

#############################
## PLOT_POSITION_FREQUENCY ##
#############################

pos_frq_plot <- plot_position_frequency(dat, 
                                        protein = chosen_protein, 
                                        chosen_state = chosen_state)

expect_doppelganger("Position Frequency Plot", pos_frq_plot)

#############################
## PLOT_AMINO_DISTRIBUTION ##
#############################

# plot_amino_distribution # badly written

###############################
## PLOT_OVERLAP_DISTRIBUTION ##
###############################

overlap_dist_dat <- create_overlap_distribution_dataset(dat = dat, 
                                                        protein = chosen_protein,
                                                        state = chosen_state,
                                                        protein_sequence = reconstruct_sequence(dat))

overlap_dist_plot <- plot_overlap_distribution(dat = overlap_dist_dat,
                                               start = 1)

expect_doppelganger("Overlap Distribution Plot", overlap_dist_plot)



#######################
## SHOW_OVERLAP_DATA ##
#######################

result_tmp <- show_overlap_data(dat, 
                                protein = chosen_protein,
                                state = chosen_state,
                                start = min(dat[["Start"]]),
                                end = max(dat[["End"]]))
test_that("show_overlap_data colnames",
          expect_equal(colnames(result_tmp),
                       c("Protein", "Sequence", "Start", "End")
          )
)

test_that("show_overlap_data rows",
          expect_equal(nrow(result_tmp),
                       41
          )
)

##################
## PLOT_OVERLAP ##
##################

overlap_plot <- plot_overlap(result_tmp)

expect_doppelganger("Overlap Plot", overlap_plot)