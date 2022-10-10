theo_frac_r_kinetic_plot <- plot_uptake_curve(pep_kinetics_dat,
                                              theoretical = T,
                                              fractional = T,
                                              uncertainty_type = "ribbon",
                                              log_x = F)

theo_r_log_kinetic_plot <- plot_uptake_curve(pep_kinetics_dat,
                                             theoretical = T,
                                             fractional = F,
                                             uncertainty_type = "ribbon",
                                             log_x = T)

frac_b_kinetic_plot <- plot_uptake_curve(pep_kinetics_dat,
                                         theoretical = F,
                                         fractional = T,
                                         uncertainty_type = "bars",
                                         log_x = F)

bl_kinetic_plot <- plot_uptake_curve(pep_kinetics_dat,
                                     theoretical = F,
                                     fractional = F,
                                     uncertainty_type = "bars + line",
                                     log_x = F)

test_that("plot parameters",{
  vdiffr::expect_doppelganger("Theo Frac kinetic plot (ribbon)", theo_frac_r_kinetic_plot)
  vdiffr::expect_doppelganger("Theo log kinetic plot (ribbon)", theo_r_log_kinetic_plot)
  vdiffr::expect_doppelganger("Frac kinetic plot (bars)", frac_b_kinetic_plot)
  vdiffr::expect_doppelganger("kinetic plot (bars+line)", bl_kinetic_plot)
})
