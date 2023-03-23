coverage_heatmap <- plot_coverage_heatmap(uptake_dat)

test_that("class is right",
          expect_is(coverage_heatmap, "ggplot"))

test_that("plot works", 
          vdiffr::expect_doppelganger("plot works", coverage_heatmap))

# test_that("no value", 
#           expect_equal(coverage_heatmap, 
#                        plot_coverage(uptake_dat)))


frac_deut_uptake_heatmap <- plot_coverage_heatmap(uptake_dat, value = "frac_deut_uptake", time_t = t_time_t)
frac_theo_deut_uptake_heatmap <- plot_coverage_heatmap(uptake_dat, value = "theo_frac_deut_uptake", time_t = t_time_t)
theo_deut_uptake_heatmap <- plot_coverage_heatmap(uptake_dat, value = "theo_deut_uptake", time_t = t_time_t)
deut_uptake_heatmap<- plot_coverage_heatmap(uptake_dat, value = "deut_uptake", time_t = t_time_t)
err_frac_deut_uptake_heatmap <- plot_coverage_heatmap(uptake_dat, value = "err_frac_deut_uptake", time_t = t_time_t)
err_frac_theo_deut_uptake_heatmap <- plot_coverage_heatmap(uptake_dat, value = "err_theo_frac_deut_uptake", time_t = t_time_t)
err_theo_deut_uptake_heatmap <- plot_coverage_heatmap(uptake_dat, value = "err_theo_deut_uptake", time_t = t_time_t)
err_deut_uptake_heatmap<- plot_coverage_heatmap(uptake_dat, value = "err_deut_uptake", time_t = t_time_t)

test_that("uptake values", {
  vdiffr::expect_doppelganger("frac_uptake_heatmap", frac_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("frac_theo_uptake_heatmap", frac_theo_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("theo_uptake_heatmap", theo_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("uptake_heatmap", deut_uptake_heatmap)
  vdiffr::expect_doppelganger("err_frac_uptake_heatmap", err_frac_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("err_frac_theo_uptake_heatmap", err_frac_theo_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("err_theo_uptake_heatmap", err_theo_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("err_uptake_heatmap", err_deut_uptake_heatmap)
})

diff_frac_deut_uptake_heatmap <- plot_coverage_heatmap(diff_uptake_dat, value = "diff_frac_deut_uptake", time_t = t_time_t)
err_diff_frac_deut_uptake_heatmap <- plot_coverage_heatmap(diff_uptake_dat, value = "err_diff_frac_deut_uptake", time_t = t_time_t)
diff_deut_uptake_heatmap <- plot_coverage_heatmap(diff_uptake_dat, value = "diff_deut_uptake", time_t = t_time_t)
err_diff_deut_uptake_heatmap <- plot_coverage_heatmap(diff_uptake_dat, value = "err_diff_deut_uptake", time_t = t_time_t)
diff_theo_frac_deut_uptake_heatmap <- plot_coverage_heatmap(diff_uptake_dat, value = "diff_theo_frac_deut_uptake", time_t = t_time_t)
err_diff_theo_frac_deut_uptake_heatmap <- plot_coverage_heatmap(diff_uptake_dat, value = "err_diff_theo_frac_deut_uptake", time_t = t_time_t)
diff_theo_deut_uptake_heatmap <- plot_coverage_heatmap(diff_uptake_dat, value = "diff_theo_deut_uptake", time_t = t_time_t)
err_diff_theo_deut_uptake_heatmap <- plot_coverage_heatmap(diff_uptake_dat, value = "err_diff_theo_deut_uptake", time_t = t_time_t)

test_that("diff uptake values", {
  vdiffr::expect_doppelganger("diff_frac_deut_uptake_heatmap", diff_frac_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("diff_frac_theo_uptake_heatmap", diff_theo_frac_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("diff_theo_uptake_heatmap", diff_theo_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("diff_uptake_heatmap", diff_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("diff_err_frac_uptake_heatmap", err_diff_frac_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("diff_err_frac_theo_uptake_heatmap", err_diff_theo_frac_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("diff_err_theo_uptake_heatmap", err_diff_theo_deut_uptake_heatmap)
  vdiffr::expect_doppelganger("diff_err_uptake_heatmap", err_diff_deut_uptake_heatmap)
})

test_that("message",
          expect_message(plot_coverage_heatmap(diff_uptake_dat, value = "diff_deut_uptake")))

auc_heatmap <- plot_coverage_heatmap(auc_dat, value = "auc")

test_that("auc plot",
          vdiffr::expect_doppelganger("auc", auc_heatmap))

bx_heatmap <- plot_coverage_heatmap(bx_dat, value = "back_exchange")

test_that("baxk_exchange plot", 
          vdiffr::expect_doppelganger("back exchange", bx_heatmap))