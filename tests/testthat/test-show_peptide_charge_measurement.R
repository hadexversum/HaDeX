test_that("show_peptide_charge_measurement class is right",
          expect_is(
            show_peptide_charge_measurement(t_dat),
            "data.frame"
          )
)

test_that("show_peptide_charge_measurement colnames",
          expect_equal(
            colnames(show_peptide_charge_measurement(t_dat)),
            c("Protein", "Sequence", "Start", "End", "State", "Exposure",  "File", "z")
          )
)
