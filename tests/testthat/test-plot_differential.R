test_that("returns ggplot object",
          expect_s3_class(plot_differential(diff_uptake_dat), 
                          "ggplot"))


theoretical_differential_plot <- plot_differential(diff_uptake_dat, 
                                                   theoretical = T)

fractional_differential_plot <- plot_differential(diff_uptake_dat, 
                                                  fractional = T)

theo_frac_diff_plot <- plot_differential(diff_uptake_dat, 
                                         theoretical = T, 
                                         fractional = T)

test_that("calculation parameters", {
  vdiffr::expect_doppelganger("theoretical differential plot", theoretical_differential_plot)
  vdiffr::expect_doppelganger("fractional differential plot", fractional_differential_plot)
  vdiffr::expect_doppelganger("theo frac differential plot", theo_frac_diff_plot)
})

houde_differential_plot <- plot_differential(diff_uptake_dat, 
                                             show_houde_interval = T,
                                             hide_houde_insignificant = T)

test_that("houde test results",
          vdiffr::expect_doppelganger("differential with houde", houde_differential_plot))

tstudent_differential_plot <- plot_differential(diff_uptake_dat, 
                                                diff_p_uptake_dat,
                                                show_tstud_confidence = T, 
                                                hide_tstud_insignificant = T)

test_that("tstudent test results",
          vdiffr::expect_doppelganger("differential with tstudent", tstudent_differential_plot))

all_differential_plot <- plot_differential(diff_p_uptake_dat = diff_p_uptake_dat,
                                           all_times = T)

test_that("differential plot in all times", 
          vdiffr::expect_doppelganger("differential plot all times", all_differential_plot))

insig_tsutd_diff_plot <- plot_differential(diff_uptake_dat, 
                                           diff_p_uptake_dat,
                                           show_tstud_confidence = T,
                                           hide_tstud_insignificant = F)

test_that("showing / hiding",
          vdiffr::expect_doppelganger("insig / sig tstud", insig_tsutd_diff_plot))

skip_amino_diff_plot <- plot_differential(diff_uptake_dat, 
                                          skip_amino = 1)

test_that("skip amino",
          vdiffr::expect_doppelganger("skip amino", skip_amino_diff_plot))

## TODO: supplied data
## TODO: lack time_t
