test_that("returns ggplot object",
          expect_s3_class(plot_differential_chiclet(diff_uptake_dat), 
                          "ggplot"))

theo_frac_u_differential_chiclet_plot <- plot_differential_chiclet(diff_uptake_dat,
                                                                   theoretical = T, 
                                                                   fractional = T, 
                                                                   show_uncertainty = T)

frac_u_differential_chiclet_plot <- plot_differential_chiclet(diff_uptake_dat,
                                                              theoretical = F, 
                                                              fractional = T, 
                                                              show_uncertainty = T)

theo_differential_chiclet_plot <- plot_differential_chiclet(diff_uptake_dat,
                                                            theoretical = T, 
                                                            fractional = F, 
                                                            show_uncertainty = F)

differential_chiclet_plot <- plot_differential_chiclet(diff_uptake_dat,
                                                       theoretical = F, 
                                                       fractional = F, 
                                                       show_uncertainty = F)
test_that("plot_parameters", {
  vdiffr::expect_doppelganger("Theo Frac Differential Chiclet Plot + uncertainty ", theo_frac_u_differential_chiclet_plot)
  vdiffr::expect_doppelganger("Frac Differential Chiclet Plot + uncertainty ", frac_u_differential_chiclet_plot)
  vdiffr::expect_doppelganger("Theo Differential Chiclet Plot ", theo_differential_chiclet_plot)
  vdiffr::expect_doppelganger("Differential Chiclet Plot ", differential_chiclet_plot)
})

hybrid_differential_chiclet_plot <- plot_differential_chiclet(diff_p_uptake_dat = diff_p_uptake_dat, 
                                                              show_houde_interval = T, 
                                                              show_tstud_confidence = T,
                                                              show_uncertainty = T)

test_that("hybrid test", 
          vdiffr::expect_doppelganger("hybrid test", hybrid_differential_chiclet_plot))

## TODO supplied data