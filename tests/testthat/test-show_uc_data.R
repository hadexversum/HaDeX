test_that("Theo Frac colnames in show_kinetic_data",
          expect_equal(
            colnames(
              show_uc_data(pep_kinetics_dat, 
                           fractional = T, 
                           theoretical = T)
            ),
            c("Protein", "Sequence", "State", "Start", "End", "Time Point", "Theo Frac DU [%]", "Theo Err Frac DU [%]")
          )
)

test_that("Theo colnames in show_kinetic_data",
          expect_equal(
            colnames(
              show_uc_data(pep_kinetics_dat, 
                           fractional = F, 
                           theoretical = T)
            ),
            c("Protein", "Sequence", "State", "Start", "End", "Time Point", "Theo DU [Da]", "Theo Err DU [Da]")
          )
)

test_that("Frac colnames in show_kinetic_data",
          expect_equal(
            colnames(
              show_uc_data(pep_kinetics_dat, 
                           fractional = T, 
                           theoretical = F)
            ),
            c("Protein", "Sequence", "State", "Start", "End", "Time Point", "Frac DU [%]", "Err Frac DU [%]")
          )
)

test_that("colnames in show_kinetic_data",
          expect_equal(
            colnames(
              show_uc_data(pep_kinetics_dat, 
                           fractional = F, 
                           theoretical = F)
            ),
            c("Protein", "Sequence", "State", "Start", "End", "Time Point", "DU [Da]", "Err DU [Da]")
          )
)