simple_diff_uptake_curve <- plot_differential_uptake_curve(diff_uptake_dat = diff_uptake_dat, sequence = t_peptide)

test_that("class is right",
          expect_is(simple_diff_uptake_curve, "ggplot"))

test_that("plot works", 
          vdiffr::expect_doppelganger("diff uptake curve", simple_diff_uptake_curve))


frac_theo_diff_uptake_curve <- plot_differential_uptake_curve(diff_uptake_dat = diff_uptake_dat, 
                                                              sequence = t_peptide,
                                                              theoretical = T,
                                                              fractional = T)

theo_diff_uptake_curve <- plot_differential_uptake_curve(diff_uptake_dat = diff_uptake_dat, 
                                                         sequence = t_peptide,
                                                         theoretical = T,
                                                         fractional = F)

frac_diff_uptake_curve <- plot_differential_uptake_curve(diff_uptake_dat = diff_uptake_dat, 
                                                         sequence = t_peptide,
                                                         theoretical = F,
                                                         fractional = T)

test_that("calculation params", {
  vdiffr::expect_doppelganger("theo frac diff uc", frac_theo_diff_uptake_curve)
  vdiffr::expect_doppelganger("theo diff uc", theo_diff_uptake_curve)
  vdiffr::expect_doppelganger("frac diff uc", frac_diff_uptake_curve)
})

p_diff_uptake_curve <- plot_differential_uptake_curve(diff_p_uptake_dat = diff_p_uptake_dat,
                                                      uncertainty_type = "bars + line",
                                                      sequence = t_peptide,
                                                      show_houde_interval = T,
                                                      show_tstud_confidence = T)

test_that("hybrid test on diff cu (bars + line)",
          vdiffr::expect_doppelganger("hybrid test", p_diff_uptake_curve))

test_that("no data", 
          expect_error(plot_differential_uptake_curve()))

no_log_bars_diff_uptake_curve <- plot_differential_uptake_curve(diff_uptake_dat = diff_uptake_dat, 
                                                                sequence = t_peptide, 
                                                                log_x = F,
                                                                uncertainty_type = "bars")
test_that("no logx (bars)", 
          vdiffr::expect_doppelganger("no logx", no_log_bars_diff_uptake_curve))