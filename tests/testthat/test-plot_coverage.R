coverage_plot <- plot_coverage(t_dat, states = t_state_1)

test_that("class is right",
          expect_is(coverage_plot,
                    "ggplot"))

test_that("plot works", 
          vdiffr::expect_doppelganger("plot works", coverage_plot))
