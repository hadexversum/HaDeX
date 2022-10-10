test_that("create_replicate_dataset class is right",
          expect_is(
            rep_dat,
            "data.frame"
          )
)

test_that("create_replicate_dataset colnames",
          expect_equal(
            colnames(rep_dat),
            c("Sequence", "Exposure", "Start", "End", "ID", "n")
          )
)

test_that("create_replicate_dataset dimention",
          expect_equal(
            dim(rep_dat),
            c(328, 6)
          )
          
)

rep_dat_ref <- structure(list(Sequence = c("INITSSASQEGTRLN", "INITSSASQEGTRLN", 
                                       "INITSSASQEGTRLN", "INITSSASQEGTRLN", 
                                       "INITSSASQEGTRLN", "INITSSASQEGTRLN", 
                                       "INITSSASQEGTRLN", "INITSSASQEGTRLN"), 
                          Exposure = c(0, 0.001,  0.167, 1, 5, 25, 120, 1440), 
                          Start = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                          End = c(15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L), 
                          ID = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), 
                          n = c(1L, 1L, 4L, 4L, 4L, 4L, 4L, 4L)), 
                     row.names = c(NA, -8L), 
                     class = c("tbl_df", "tbl", "data.frame"), 
                     state = "CD160")

test_that("create_replicate_dataset values",
          expect_equal(
            data.frame(rep_dat_ref),
            data.frame(rep_dat[rep_dat[["Sequence"]] == "INITSSASQEGTRLN", ])
          )
)
