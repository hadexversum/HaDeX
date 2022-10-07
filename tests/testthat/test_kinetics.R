dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

chosen_protein <- "db_CD160"
chosen_state <- "CD160"
chosen_time_0 <- 0.001
chosen_time_100 <- 1440
chosen_time <- 5
deut_part <- 1

chosen_peptide <- "INITSSASQEGTRLN"
chosen_peptide_start <- 1 
chosen_peptide_end <- 15

states <- unique(dat[["State"]])

result_tmp <- calculate_peptide_kinetics(dat, 
                                         protein = chosen_protein,
                                         sequence = chosen_peptide, 
                                         states = states, 
                                         start = chosen_peptide_start, 
                                         end = chosen_peptide_end,
                                         time_0 = chosen_time_0,
                                         time_100 = chosen_time_100,
                                         deut_part = deut_part)

#######################
## SHOW_KINETIC_DATA ##
#######################

test_that("Theo Frac colnames in show_kinetic_data",
          expect_equal(
            colnames(
              show_uc_data(result_tmp, 
                          fractional = T, 
                          theoretical = T)
            ),
            c("Protein", "Sequence", "State", "Start", "End", "Time Point", "Theo Frac DU [%]", "Theo Err Frac DU [%]")
          )
)

test_that("Theo colnames in show_kinetic_data",
          expect_equal(
            colnames(
              show_uc_data(result_tmp, 
                                fractional = F, 
                                theoretical = T)
            ),
            c("Protein", "Sequence", "State", "Start", "End", "Time Point", "Theo DU [Da]", "Theo Err DU [Da]")
          )
)

test_that("Frac colnames in show_kinetic_data",
          expect_equal(
            colnames(
              show_uc_data(result_tmp, 
                                fractional = T, 
                                theoretical = F)
            ),
            c("Protein", "Sequence", "State", "Start", "End", "Time Point", "Frac DU [%]", "Err Frac DU [%]")
          )
)

test_that("colnames in show_kinetic_data",
          expect_equal(
            colnames(
              show_uc_data(result_tmp, 
                                fractional = F, 
                                theoretical = F)
            ),
            c("Protein", "Sequence", "State", "Start", "End", "Time Point", "DU [Da]", "Err DU [Da]")
          )
)

###################
## PLOT_KINETICS ##
###################

theo_frac_r_kinetic_plot <- plot_uptake_curve(result_tmp,
                                              theoretical = T,
                                              fractional = T,
                                              uncertainty_type = "ribbon",
                                              log_x = F)

expect_doppelganger("Theo Frac kinetic plot (ribbon)", theo_frac_r_kinetic_plot)

theo_r_log_kinetic_plot <- plot_uptake_curve(result_tmp,
                                             theoretical = T,
                                             fractional = F,
                                             uncertainty_type = "ribbon",
                                             log_x = T)

expect_doppelganger("Theo log kinetic plot (ribbon)", theo_r_log_kinetic_plot)

frac_b_kinetic_plot <- plot_uptake_curve(result_tmp,
                                         theoretical = F,
                                         fractional = T,
                                         uncertainty_type = "bars",
                                         log_x = F)

expect_doppelganger("Frac kinetic plot (bars)", frac_b_kinetic_plot)

bl_kinetic_plot <- plot_uptake_curve(result_tmp,
                                     theoretical = F,
                                     fractional = F,
                                     uncertainty_type = "bars + line",
                                     log_x = F)

expect_doppelganger("kinetic plot (bars+line)", bl_kinetic_plot)

