test_that("Theo Frac show_diff_uptake_data colnames",
          expect_equal(colnames(show_diff_uptake_data(diff_uptake_dat,
                                                      theoretical = T,
                                                      fractional = T)),
                       c("Protein", "ID", "Sequence", "Modification", "Start", "End", "Exposure", "Theo Frac Diff DU [%]", "U(Theo Frac Diff DU) [%]")
          )
)

test_that("Frac show_diff_uptake_data colnames",
          expect_equal(colnames(show_diff_uptake_data(diff_uptake_dat,
                                                      theoretical = F,
                                                      fractional = T)),
                       c("Protein", "ID", "Sequence", "Modification", "Start", "End", "Exposure", "Frac Diff DU [%]", "U(Frac Diff DU) [%]")
          )
)

test_that("Theo show_diff_uptake_data colnames",
          expect_equal(colnames(show_diff_uptake_data(diff_uptake_dat,
                                                      theoretical = T,
                                                      fractional = F)),
                       c("Protein", "ID", "Sequence", "Modification", "Start", "End", "Exposure", "Theo Diff DU [Da]", "U(Theo Diff DU) [Da]")
          )
)

test_that("show_diff_uptake_data colnames",
          expect_equal(colnames(show_diff_uptake_data(diff_uptake_dat,
                                                      theoretical = F,
                                                      fractional = F)),
                       c("Protein", "ID", "Sequence", "Modification", "Start", "End", "Exposure", "Diff DU [Da]", "U(Diff DU) [Da]")
          )
)