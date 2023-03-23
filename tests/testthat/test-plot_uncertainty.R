uncertainty_plot <- plot_uncertainty(t_dat)

test_that("class is right",
          expect_is(uncertainty_plot, "ggplot"))

test_that("plot works",
          vdiffr::expect_doppelganger("uncertainty plot", uncertainty_plot))

params_uncertainty_plot <- plot_uncertainty(t_dat,
                                            skip_amino = 1)

test_that("plot parameters",
          vdiffr::expect_doppelganger("plot params", params_uncertainty_plot))

## TODO aggregated = F
