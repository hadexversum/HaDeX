show_tmp <- show_overlap_data(t_dat, 
                              protein = t_protein,
                              state = t_state_1,
                              start = t_start,
                              end = t_end)

test_that("show_overlap_data colnames",
          expect_equal(colnames(show_tmp),
                       c("Protein", "Sequence", "ID", "Start", "End")
          )
)

test_that("show_overlap_data rows",
          expect_equal(nrow(show_tmp), 41))