peptide_mass_plot <- plot_peptide_mass_measurement(t_dat)

test_that("plot_peptide_mass_measurement class is right",
          expect_is(
            peptide_mass_plot,
            "ggplot"
          )
)

test_that("plot parameters",
          vdiffr::expect_doppelganger("Peptide Mass Measurement Plot", peptide_mass_plot))
