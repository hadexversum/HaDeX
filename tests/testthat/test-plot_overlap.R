overlap_plot <- plot_overlap(t_dat, protein = t_protein, state = t_state_1)

test_that("class is right", 
          expect_is(overlap_plot, "ggplot"))

test_that("plot works", 
          vdiffr::expect_doppelganger("overlap plot", overlap_plot))