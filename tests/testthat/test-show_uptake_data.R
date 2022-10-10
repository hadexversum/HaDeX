test_that("Theo Frac show_uptake_data colnames",
          expect_equal(colnames(show_uptake_data(uptake_dat,
                                                 theoretical = T,
                                                 fractional = T)),
                       c("Protein", "Sequence", "ID", "Modification", "State", "Start", "End", "Exposure", "Theo Frac DU [%]", "U(Theo Frac DU) [%]")
          )
)

test_that("Frac show_uptake_data colnames",
          expect_equal(colnames(show_uptake_data(uptake_dat,
                                                 theoretical = F,
                                                 fractional = T)),
                       c("Protein", "Sequence", "ID", "Modification", "State", "Start", "End", "Exposure", "Frac DU [%]", "U(Frac DU) [%]")
          )
)

test_that("Theo show_uptake_data colnames",
          expect_equal(colnames(show_uptake_data(uptake_dat,
                                                 theoretical = T,
                                                 fractional = F)),
                       c("Protein", "Sequence", "ID", "Modification", "State", "Start", "End", "Exposure", "Theo DU [Da]", "U(Theo DU) [Da]")
          )
)

test_that("show_uptake_data colnames",
          expect_equal(colnames(show_uptake_data(uptake_dat,
                                                 theoretical = F,
                                                 fractional = F)),
                       c("Protein", "Sequence", "ID", "Modification", "State", "Start", "End", "Exposure", "DU [Da]", "U(DU) [Da]")
          )
)