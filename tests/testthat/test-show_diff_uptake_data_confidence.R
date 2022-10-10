test_that("Theo Frac show_diff_uptake_data_confidence colnames",
          expect_equal(colnames(show_diff_uptake_data_confidence(diff_uptake_dat,
                                                                 theoretical = T,
                                                                 fractional = T)),
                       c("Protein", "Sequence", "ID", "Modification","Start", "End", "Exposure", "Theo Frac Diff DU [%]", "U(Theo Frac Diff DU) [%]", "Valid At 0.98")
          )
)

test_that("Theo show_diff_uptake_data_confidence colnames",
          expect_equal(colnames(show_diff_uptake_data_confidence(diff_uptake_dat,
                                                                 theoretical = T,
                                                                 fractional = F)),
                       c("Protein", "Sequence", "ID", "Modification","Start", "End", "Exposure", "Theo Diff DU [Da]", "U(Theo Diff DU) [Da]", "Valid At 0.98")
          )
)

test_that("Frac show_diff_uptake_data_confidence colnames",
          expect_equal(colnames(show_diff_uptake_data_confidence(diff_uptake_dat,
                                                                 theoretical = F,
                                                                 fractional = T)),
                       c("Protein", "Sequence", "ID", "Modification","Start", "End", "Exposure", "Frac Diff DU [%]", "U(Frac Diff DU) [%]", "Valid At 0.98")
          )
)

test_that("show_diff_uptake_data_confidence colnames",
          expect_equal(colnames(show_diff_uptake_data_confidence(diff_uptake_dat,
                                                                 theoretical = F,
                                                                 fractional = F)),
                       c("Protein", "Sequence", "ID", "Modification","Start", "End", "Exposure", "Diff DU [Da]", "U(Diff DU) [Da]" , "Valid At 0.98")
          )
)