simple_manhattan_plot <- plot_manhattan(diff_p_uptake_dat)

test_that("class is right",
          expect_is(simple_manhattan_plot, "ggplot"))

test_that("plot works",
          vdiffr::expect_doppelganger("manhattan plot", simple_manhattan_plot))

params_manhattan_plot <- plot_manhattan(diff_p_uptake_dat, 
                                        skip_amino = 1,
                                        show_peptide_position = T)

test_that("plot_params", 
          vdiffr::expect_doppelganger("plot params", params_manhattan_plot))