pos_frq_plot <- plot_position_frequency(t_dat, 
                                        protein = t_protein, 
                                        state = t_state_1)

test_that("plot_position_frequency plot",
          vdiffr::expect_doppelganger("Position Frequency Plot", pos_frq_plot))
