qc_dat <- create_quality_control_dataset(dat = t_dat,
                                         protein = t_protein, 
                                         state_1 = t_state_1,
                                         state_2 = t_state_2, 
                                         time_t = 1, 
                                         time_0 = t_time_0, 
                                         deut_part = t_deut_part)

test_that("class is right",
          expect_is(qc_dat, "data.frame"))

test_that("size is right",
          expect_equal(length(colnames(qc_dat)), 7))

show_qc_dat <- show_quality_control_data(qc_dat)

test_that("show_result class is right",
          expect_is(show_qc_dat, "data.frame"))

test_that("show_result size is right",
          expect_equal(length(colnames(show_qc_dat)), 4))

quality_control_plot <- plot_quality_control(qc_dat)

test_that("class is right",
          expect_is(quality_control_plot, "ggplot"))

test_that("plot works",
          vdiffr::expect_doppelganger("Quality control Plot", quality_control_plot))
