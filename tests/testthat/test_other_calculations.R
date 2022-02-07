dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

chosen_protein <- "db_CD160"
chosen_state <- "CD160"

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
                       c("Protein", "Sequence", "ID", "Start", "End")
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
