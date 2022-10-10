peptide_charge_plot <- plot_peptide_charge_measurement(t_dat)

test_that("plot_peptide_charge_measurement class is right",
          expect_is(
            peptide_charge_plot,
            "ggplot"
          )
)

test_that("plot parameters",
          vdiffr:: expect_doppelganger("Peptide Charge Measurement Plot", peptide_charge_plot))

