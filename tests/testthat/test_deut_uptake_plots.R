context("deut_uptake plots in test")

library(vdiffr)

dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))

chosen_protein <- "db_CD160"
chosen_states <- c("CD160", "CD160_HVEM")
chosen_state <- "CD160"
chosen_time_0 <- 0.001
chosen_time_100 <- 1440
chosen_time <- 5
deut_part <- 1

chosen_confidence_level  <- 0.98
chosen_confidence_level_2 <- 0.99


#########################
#########################
###### DEUT_UPTAKE ######
#########################
#########################

state_uptake_dat <- create_state_uptake_dataset(dat,
                                                protein = chosen_protein, 
                                                state = chosen_state, 
                                                time_0 = chosen_time_0,
                                                time_100 = chosen_time_100,
                                                deut_part = deut_part)


####################
## PLOT_BUTTERFLY ##
####################

theo_frac_r_butterfly_plot <- plot_butterfly(state_uptake_dat,
                                             theoretical = T,
                                             fractional = T,
                                             uncertainty_type = "ribbon")

expect_doppelganger("Theo Frac butterfly plot (ribbon)", theo_frac_r_butterfly_plot)

frac_r_butterfly_plot <- plot_butterfly(state_uptake_dat,
                                        theoretical = F,
                                        fractional = T,
                                        uncertainty_type = "ribbon")

expect_doppelganger("Frac butterfly plot (ribbon)", frac_r_butterfly_plot)

theo_b_butterfly_plot <- plot_butterfly(state_uptake_dat,
                                        theoretical = T,
                                        fractional = F,
                                        uncertainty_type = "bars")

expect_doppelganger("Theo Frac butterfly plot (bars)", theo_b_butterfly_plot)

bl_butterfly_plot <- plot_butterfly(state_uptake_dat,
                                    theoretical = F,
                                    fractional = F,
                                    uncertainty_type = "bars + line")

expect_doppelganger("Butterfly plot (bars + line)", bl_butterfly_plot)

##################
## PLOT_CHICLET ##
##################

theo_frac_chiclet_plot <- plot_chiclet(state_uptake_dat,
                                       theoretical = T,
                                       fractional = T,
                                       show_uncertainty = F)

expect_doppelganger("Theo Frac Chiclet plot", theo_frac_chiclet_plot)


frac_u_chiclet_plot <- plot_chiclet(state_uptake_dat,
                                    theoretical = F,
                                    fractional = T,
                                    show_uncertainty = T)

expect_doppelganger("Frac Chiclet plot + uncertainty", frac_u_chiclet_plot)


theo_chiclet_plot <- plot_chiclet(state_uptake_dat,
                                  theoretical = T,
                                  fractional = F,
                                  show_uncertainty = F)

expect_doppelganger("Theo Chiclet plot", theo_chiclet_plot)


u_chiclet_plot <- plot_chiclet(state_uptake_dat,
                               theoretical = F,
                               fractional = F,
                               show_uncertainty = T)

expect_doppelganger("Chiclet plot + uncertainty", u_chiclet_plot)

###########################
## PLOT_STATE_COMPARISON ##
###########################

theo_frac_state_comparison_plot <- plot_state_comparison(dat = state_uptake_dat[state_uptake_dat[["Exposure"]] == chosen_time, ],
                                                         theoretical = T, 
                                                         fractional = T)

expect_doppelganger("Theo Frac State Comparison plot", theo_frac_state_comparison_plot)

frac_state_comparison_plot <- plot_state_comparison(dat = state_uptake_dat[state_uptake_dat[["Exposure"]] == chosen_time, ],
                                                    theoretical = F, 
                                                    fractional = T)

expect_doppelganger("Frac State Comparison plot", frac_state_comparison_plot)

theo_state_comparison_plot <- plot_state_comparison(dat = state_uptake_dat[state_uptake_dat[["Exposure"]] == chosen_time, ],
                                                    theoretical = T, 
                                                    fractional = F)

expect_doppelganger("Theo State Comparison plot", theo_state_comparison_plot)

state_comparison_plot <- plot_state_comparison(dat = state_uptake_dat[state_uptake_dat[["Exposure"]] == chosen_time, ],
                                               theoretical = F, 
                                               fractional = F)

expect_doppelganger("State Comparison plot", state_comparison_plot)

#########################
#########################
### DIFF_DEUT_UPTAKE ####
#########################
#########################

diff_uptake_dat <- create_diff_uptake_dataset(dat = dat,
                                              protein = chosen_protein, 
                                              state_1 = chosen_states[1],
                                              state_2 = chosen_states[2],
                                              time_0 = chosen_time_0,
                                              time_100 = chosen_time_100,
                                              deut_part = deut_part)
  
#######################
## PLOT_DIFFERENTIAL ##
#######################

theo_frac_2_differential_plot <- plot_differential(dat = diff_uptake_dat[diff_uptake_dat[["Exposure"]] == chosen_time, ],
                                                   theoretical = T,
                                                   fractional = T,
                                                   confidence_level = chosen_confidence_level,
                                                   confidence_level_2 = chosen_confidence_level_2)

expect_doppelganger("Theo Frac Differential Plot (2I) ", theo_frac_2_differential_plot)

theo_2_differential_plot <- plot_differential(dat = diff_uptake_dat[diff_uptake_dat[["Exposure"]] == chosen_time, ],
                                              theoretical = T,
                                              fractional = F,
                                              confidence_level = chosen_confidence_level,
                                              confidence_level_2 = chosen_confidence_level_2)

expect_doppelganger("Theo Differential Plot (2I) ", theo_2_differential_plot)

frac_1_differential_plot <- plot_differential(dat = diff_uptake_dat[diff_uptake_dat[["Exposure"]] == chosen_time, ],
                                              theoretical = F,
                                              fractional = T,
                                              confidence_level = chosen_confidence_level,
                                              confidence_level_2 = chosen_confidence_level)

expect_doppelganger("Frac Differential Plot (1I) ", frac_1_differential_plot)

differential_plot <- plot_differential(dat = diff_uptake_dat[diff_uptake_dat[["Exposure"]] == chosen_time, ],
                                       theoretical = F,
                                       fractional = F,
                                       confidence_level = chosen_confidence_level,
                                       confidence_level_2 = chosen_confidence_level)

expect_doppelganger("Differential Plot (1I) ", differential_plot)

###############################
## PLOT_DIFFERENTIAL_CHICLET ##
###############################

theo_frac_u_differential_chiclet_plot <- plot_differential_chiclet(diff_uptake_dat,
                                                                   theoretical = T, 
                                                                   fractional = T, 
                                                                   show_uncertainty = T)

expect_doppelganger("Theo Frac Differential Chiclet Plot + uncertainty ", theo_frac_u_differential_chiclet_plot)

frac_u_differential_chiclet_plot <- plot_differential_chiclet(diff_uptake_dat,
                                                              theoretical = F, 
                                                              fractional = T, 
                                                              show_uncertainty = T)

expect_doppelganger("Frac Differential Chiclet Plot + uncertainty ", frac_u_differential_chiclet_plot)

theo_differential_chiclet_plot <- plot_differential_chiclet(diff_uptake_dat,
                                                            theoretical = T, 
                                                            fractional = F, 
                                                            show_uncertainty = F)

expect_doppelganger("Theo Differential Chiclet Plot ", theo_differential_chiclet_plot)

differential_chiclet_plot <- plot_differential_chiclet(diff_uptake_dat,
                                                       theoretical = F, 
                                                       fractional = F, 
                                                       show_uncertainty = F)

expect_doppelganger("Differential Chiclet Plot ", differential_chiclet_plot)

#################################
## PLOT_DIFFERENTIAL_BUTTERFLY ##
#################################

theo_frac_differential_butterfly_plot <- plot_differential_butterfly(diff_uptake_dat, 
                                                                     theoretical = T,
                                                                     fractional = T,
                                                                     uncertainty = "ribbon",
                                                                     show_confidence_limit = T,
                                                                     confidence_level = chosen_confidence_level)

expect_doppelganger("Theo Frac Differential Butterfly Plot (ribbon)", theo_frac_differential_butterfly_plot)

frac_differential_butterfly_plot <- plot_differential_butterfly(diff_uptake_dat, 
                                                                     theoretical = F,
                                                                     fractional = T,
                                                                     uncertainty = "bars",
                                                                     show_confidence_limit = T,
                                                                     confidence_level = chosen_confidence_level)

expect_doppelganger("Frac Differential Butterfly Plot (bars)", frac_differential_butterfly_plot)

theo_differential_butterfly_plot <- plot_differential_butterfly(diff_uptake_dat, 
                                                                     theoretical = T,
                                                                     fractional = F,
                                                                     uncertainty = "bars + line",
                                                                     show_confidence_limit = F,
                                                                     confidence_level = chosen_confidence_level)

expect_doppelganger("Theo Differential Butterfly Plot (bars + line)", theo_differential_butterfly_plot)

differential_butterfly_plot <- plot_differential_butterfly(diff_uptake_dat, 
                                                                     theoretical = F,
                                                                     fractional = F,
                                                                     uncertainty = "ribbon",
                                                                     show_confidence_limit = F,
                                                                     confidence_level = chosen_confidence_level)

expect_doppelganger("Differential Butterfly Plot", differential_butterfly_plot)


# "plot_amino_distribution"     ""              ""                "plot_coverage"               ""          
# "" ""   "plot_kinetics"               "plot_overlap"                "plot_overlap_distribution"  
# "plot_position_frequency"     "plot_quality_control"        ""       "plot_volcano"               
