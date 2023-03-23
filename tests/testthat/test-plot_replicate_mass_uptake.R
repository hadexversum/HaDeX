replicate_mass_uptake_plot <- plot_replicate_mass_uptake(t_dat)

test_that("class is right",
          expect_is(replicate_mass_uptake_plot, "ggplot"))

test_that("plot works",
          vdiffr::expect_doppelganger("replicate mass uptake plot", replicate_mass_uptake_plot))

agg_replicate_mass_uptake_plot <- plot_replicate_mass_uptake(t_dat, 
                                                             aggregated = T, 
                                                             log_x = F)

test_that("plot parameters", 
          vdiffr::expect_doppelganger("aggregated plot", agg_replicate_mass_uptake_plot))