test_that("returns ggplot object",
          expect_s3_class(plot_butterfly(state_uptake_dat), 
                          "ggplot"))

theo_frac_r_butterfly_plot <- plot_butterfly(state_uptake_dat,
                                             theoretical = T,
                                             fractional = T,
                                             uncertainty_type = "ribbon")

frac_r_butterfly_plot <- plot_butterfly(state_uptake_dat,
                                        theoretical = F,
                                        fractional = T,
                                        uncertainty_type = "ribbon")

theo_b_butterfly_plot <- plot_butterfly(state_uptake_dat,
                                        theoretical = T,
                                        fractional = F,
                                        uncertainty_type = "bars")

bl_butterfly_plot <- plot_butterfly(state_uptake_dat,
                                    theoretical = F,
                                    fractional = F,
                                    uncertainty_type = "bars + line")

test_that("plot parameters",{
  vdiffr::expect_doppelganger("Theo Frac butterfly plot (ribbon)", theo_frac_r_butterfly_plot)
  vdiffr::expect_doppelganger("Frac butterfly plot (ribbon)", frac_r_butterfly_plot)
  vdiffr::expect_doppelganger("Theo Frac butterfly plot (bars)", theo_b_butterfly_plot)
  vdiffr::expect_doppelganger("Butterfly plot (bars + line)", bl_butterfly_plot)
})