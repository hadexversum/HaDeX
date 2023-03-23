t_dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
t_dat_2 <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_190304_Nucb2_EDTA_CaCl2_test02_clusterdata.csv"))


############
## PARAMS ##
############

t_protein <- "db_CD160"
t_states <- c("CD160", "CD160_HVEM")
t_state_1 <- "CD160"
t_state_2 <- "CD160_HVEM"
t_time_0 <- 0.001
t_time_100 <- 1440
t_time_t <- 5
t_deut_part <- 0.9

t_p_adjustment_method <- "none"
t_confidence_level  <- 0.98

t_start <- min(t_dat[["Start"]])
t_end <- max(t_dat[["End"]])

t_peptide <- "INITSSASQEGTRLN"
t_peptide_start <- 1 
t_peptide_end <- 15

state_uptake_dat <- create_state_uptake_dataset(t_dat,
                                                protein = t_protein, 
                                                state = t_state_1, 
                                                time_0 = t_time_0,
                                                time_100 = t_time_100,
                                                deut_part = t_deut_part)

diff_uptake_dat <- create_diff_uptake_dataset(t_dat,
                                              protein = t_protein, 
                                              state_1 = t_state_1, 
                                              state_2 = t_state_2, 
                                              time_0 = t_time_0, 
                                              time_100 = t_time_100, 
                                              deut_part = t_deut_part)


diff_p_uptake_dat <- create_p_diff_uptake_dataset(t_dat,
                                                  protein = t_protein, 
                                                  state_1 = t_state_1, 
                                                  state_2 = t_state_2,
                                                  p_adjustment_method = t_p_adjustment_method,
                                                  confidence_level = t_confidence_level,
                                                  time_0 = t_time_0, 
                                                  time_100 = t_time_100,
                                                  deut_part = t_deut_part)

p_dat <- calculate_p_value(t_dat, 
                           protein = t_protein, 
                           state_1 = t_state_1, 
                           state_2 = t_state_2,
                           p_adjustment_method = t_p_adjustment_method, 
                           confidence_level = t_confidence_level)

uptake_dat <- create_uptake_dataset(t_dat,
                                    protein = t_protein,
                                    states = t_states,
                                    time_0 = t_time_0,
                                    time_100 = t_time_100)

auc_dat <- calculate_auc(uptake_dat,
                         protein = t_protein, 
                         state = t_state_1, 
                         preserve_values = F)

bx_dat <- calculate_back_exchange(t_dat, 
                                  protein = t_protein, 
                                  states = t_states, 
                                  time_100 = t_time_100)

p_diff_uptake_conf_dat <- create_p_diff_uptake_dataset_with_confidence(diff_p_uptake_dat)

agg_test_dat <- calculate_aggregated_test_results(p_diff_uptake_conf_dat,
                                                  method = "significance")

overlap_dist_dat <- create_overlap_distribution_dataset(dat = t_dat, 
                                                        protein = t_protein,
                                                        state = t_state_1,
                                                        protein_sequence = reconstruct_sequence(t_dat))

pep_kinetics_dat <- calculate_peptide_kinetics(t_dat, 
                                               protein = t_protein,
                                               sequence = t_peptide, 
                                               states = t_states, 
                                               start = t_peptide_start, 
                                               end = t_peptide_end,
                                               time_0 = t_time_0,
                                               time_100 = t_time_100,
                                               deut_part = t_deut_part)

rep_mass_dat <- calculate_exp_masses_per_replicate(t_dat)

rep_dat <- create_replicate_dataset(t_dat)

