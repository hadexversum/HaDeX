overlap_dist_plot <- plot_overlap_distribution(overlap_dist_dat = overlap_dist_dat,
                                               start = 1)

test_that("plot_overlap_distribution works", 
          vdiffr::expect_doppelganger("Overlap Distribution Plot", overlap_dist_plot))


