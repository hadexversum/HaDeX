replicate_plot <- plot_replicate_histogram(rep_dat) 

test_that("plot_replicate_histogram class is right",
          expect_is(replicate_plot, "ggplot")
)

test_that("plot works",
          vdiffr::expect_doppelganger("Repliates histogram - all time", replicate_plot)
)

test_that("one time",
          vdiffr::expect_doppelganger("Repliates histogram - one time", plot_replicate_histogram(rep_dat[rep_dat[["Exposure"]] == 5, ])))

