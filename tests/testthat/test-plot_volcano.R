simple_volcano_plot <- plot_volcano(diff_p_uptake_dat)

test_that("class is right", 
          expect_is(simple_volcano_plot, 
                    "ggplot"))

test_that("plot works", 
          vdiffr::expect_doppelganger("volcano plot", simple_volcano_plot))

test_that("parameters conflict",
          expect_message(plot_volcano(diff_p_uptake_dat,
                                      hide_insignificant = T,
                                      show_insignificant_grey = T)))

complex_volcano_plot <- plot_volcano(diff_p_uptake_dat,
                                     adjust_axes = F,
                                     show_confidence_limits = T,
                                     color_times = F,
                                     fractional = T,
                                     theoretical = T)

last_params_volcano_plot <- plot_volcano(diff_p_uptake_dat,
                                         fractional = T,
                                         theoretical = F, 
                                         hide_insignificant = T)
test_that("parameter test", {
  vdiffr::expect_doppelganger("parameter test", complex_volcano_plot)
  vdiffr::expect_doppelganger("last params", last_params_volcano_plot)
})
          



