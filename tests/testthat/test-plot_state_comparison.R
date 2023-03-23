test_that("returns ggplot object",
          expect_s3_class(plot_state_comparison(state_uptake_dat, time_t = t_time_t), 
                          "ggplot"))


theo_frac_state_comparison_plot <- plot_state_comparison(state_uptake_dat,
                                                         theoretical = T, 
                                                         fractional = T,
                                                         time_t = t_time_t)

frac_state_comparison_plot <- plot_state_comparison(state_uptake_dat,
                                                    theoretical = F, 
                                                    fractional = T,
                                                    time_t = t_time_t)

theo_state_comparison_plot <- plot_state_comparison(state_uptake_dat, 
                                                    theoretical = T, 
                                                    fractional = F,
                                                    time_t = t_time_t)

state_comparison_plot <- plot_state_comparison(state_uptake_dat,
                                               theoretical = F, 
                                               fractional = F,
                                               time_t = t_time_t)

test_that("plot parameters", {
  vdiffr::expect_doppelganger("Theo Frac State Comparison plot", theo_frac_state_comparison_plot)
  vdiffr::expect_doppelganger("Frac State Comparison plot", frac_state_comparison_plot)
  vdiffr::expect_doppelganger("Theo State Comparison plot", theo_state_comparison_plot)
  vdiffr::expect_doppelganger("State Comparison plot", state_comparison_plot)
})