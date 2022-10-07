test_that("returns ggplot object",
          expect_s3_class(plot_chiclet(state_uptake_dat), 
                          "ggplot"))


theo_frac_chiclet_plot <- plot_chiclet(state_uptake_dat,
                                       theoretical = T,
                                       fractional = T,
                                       show_uncertainty = F)

frac_u_chiclet_plot <- plot_chiclet(state_uptake_dat,
                                    theoretical = F,
                                    fractional = T,
                                    show_uncertainty = T)

theo_chiclet_plot <- plot_chiclet(state_uptake_dat,
                                  theoretical = T,
                                  fractional = F,
                                  show_uncertainty = F)

u_chiclet_plot <- plot_chiclet(state_uptake_dat,
                               theoretical = F,
                               fractional = F,
                               show_uncertainty = T)

test_that("plot parameters",{
  vdiffr::expect_doppelganger("Theo Frac Chiclet plot", theo_frac_chiclet_plot)
  vdiffr::expect_doppelganger("Frac Chiclet plot + uncertainty", frac_u_chiclet_plot)
  vdiffr::expect_doppelganger("Theo Chiclet plot", theo_chiclet_plot)
  vdiffr::expect_doppelganger("Chiclet plot + uncertainty", u_chiclet_plot)
})