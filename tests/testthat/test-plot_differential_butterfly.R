test_that("returns ggplot object",
          expect_s3_class(plot_differential_butterfly(diff_uptake_dat), 
                          "ggplot"))

theo_frac_differential_butterfly_plot <- plot_differential_butterfly(diff_uptake_dat, 
                                                                     theoretical = T,
                                                                     fractional = T,
                                                                     uncertainty = "ribbon",
                                                                     show_houde_interval  = T,
                                                                     confidence_level = t_confidence_level)

frac_differential_butterfly_plot <- plot_differential_butterfly(diff_uptake_dat, 
                                                                theoretical = F,
                                                                fractional = T,
                                                                uncertainty = "bars",
                                                                show_houde_interval  = T,
                                                                confidence_level = t_confidence_level)

theo_differential_butterfly_plot <- plot_differential_butterfly(diff_uptake_dat, 
                                                                theoretical = T,
                                                                fractional = F,
                                                                uncertainty = "bars + line",
                                                                show_houde_interval  = F,
                                                                confidence_level = t_confidence_level)

differential_butterfly_plot <- plot_differential_butterfly(diff_uptake_dat, 
                                                           theoretical = F,
                                                           fractional = F,
                                                           uncertainty = "ribbon",
                                                           show_houde_interval  = F,
                                                           confidence_level = t_confidence_level)

test_that("plot parameters", {
  vdiffr::expect_doppelganger("Theo Frac Differential Butterfly Plot (ribbon)", theo_frac_differential_butterfly_plot)
  vdiffr::expect_doppelganger("Frac Differential Butterfly Plot (bars)", frac_differential_butterfly_plot)
  vdiffr::expect_doppelganger("Theo Differential Butterfly Plot (bars + line)", theo_differential_butterfly_plot)
  vdiffr::expect_doppelganger("Differential Butterfly Plot", differential_butterfly_plot)
})

hybrid_diff_butterfly_plot <- plot_differential_butterfly(diff_p_uptake_dat = diff_p_uptake_dat,
                                                          show_tstud_confidence = T,
                                                          show_houde_interval = T)

test_that("hybrid test", 
          vdiffr::expect_doppelganger("hybrid test", hybrid_diff_butterfly_plot))

## TODO supplied data