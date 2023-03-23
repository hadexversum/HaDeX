test_that("class is right",
          expect_is(
            show_replicate_histogram_data(rep_dat),
            "data.frame"
          )
)

test_that("show_replicate_histogram_data colnames",
          expect_equal(
            colnames(show_replicate_histogram_data(rep_dat)),
            c("Sequence", "Exposure", "Start", "End", "ID",  "n")
          )
)