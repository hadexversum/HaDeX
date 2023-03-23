test_that("show_peptide_mass_measurement class is right",
          expect_is(
            show_peptide_mass_measurement(rep_mass_dat),
            "data.frame"
          )
)

test_that("show_peptide_mass_measurement colnames",
          expect_equal(
            colnames(show_peptide_mass_measurement(rep_mass_dat)),
            c("Protein", "Sequence", "Start", "End", "Exposure", "State", "File", "Mass")
          )
)
