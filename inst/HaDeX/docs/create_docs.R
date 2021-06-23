## Let's automatize creation of helpers
## Some of the input values for different tabs in the app
## have the same meaning. For them, there are created templates
## to propagate the changes in the templates or to make creation
## of new ones easier.

library(dplyr)

## List of all of the input from the application.
## Currently, the value of input list from app (in browser option)
## is pasted here and then processed (I don't have better idea for
## now).

app_inputs <- strsplit("butt_diff_confidence_level, butt_diff_fractional, butt_diff_show_test, butt_diff_state_first, butt_diff_state_second, butt_diff_theory, butt_diff_time_0, butt_diff_time_100, butt_diff_timepoints, butt_diff_uncertainty, butt_diff_x_range, butt_diff_y_range, butt_fractional, butt_state, butt_theory, butt_time_0, butt_time_100, butt_timepoints, butt_uncertainty, butt_x_range, butt_y_range, butterflyDifferential_plot_title, butterflyDifferential_plot_title_size, butterflyDifferential_plot_x_label, butterflyDifferential_plot_x_label_size, butterflyDifferential_plot_y_label, butterflyDifferential_plot_y_label_size, butterfly_plot_title, butterfly_plot_title_size, butterfly_plot_x_label, butterfly_plot_x_label_size, butterfly_plot_y_label, butterfly_plot_y_label_size, chic_diff_fractional, chic_diff_show_uncertainty, chic_diff_state_first, chic_diff_state_second, chic_diff_theory, chic_diff_time_0, chic_diff_time_100, chic_diff_timepoints, chic_diff_x_range, chic_fractional, chic_show_uncertainty, chic_state, chic_theory, chic_time_0, chic_time_100, chic_timepoints, chic_x_range, chicletDifferential_plot_title, chicletDifferential_plot_title_size, chicletDifferential_plot_x_label, chicletDifferential_plot_x_label_size, chicletDifferential_plot_y_label, chicletDifferential_plot_y_label_size, chiclet_plot_title, chiclet_plot_title_size, chiclet_plot_x_label, chiclet_plot_x_label_size, chiclet_plot_y_label, chiclet_plot_y_label_size, chosen_control, chosen_protein, chosen_state, comp_fractional, comp_plot_y_range, compare_states, comparison_plot_title, comparison_plot_title_size, comparison_plot_x_label, comparison_plot_x_label_size, comparison_plot_y_label, comparison_plot_y_label_size, confidence_limit, confidence_limit_2, data_file, deut_part, exam_apply_changes, exam_confidence, exam_protein_name, exam_state_name, examiner_fd_timepoint, export_butterfly_differential_plot, export_butterfly_differential_plot_data, export_butterfly_plot, export_butterfly_plot_data, export_chiclet_differential_plot, export_chiclet_differential_plot_data, export_chiclet_plot, export_chiclet_plot_data, export_comparison_plot, export_comparison_plot_data, export_kin_plot, export_kin_plot_data, export_overlap_dist, export_overlap_dist_data, export_overlap_graph, export_overlap_graph_data, export_quality_control_plot, export_quality_control_plot_data, export_theo_comparison_plot, export_theo_comparison_plot_data, export_theo_kin_plot, export_theo_kin_plot_data, export_theo_woods_plot, export_theo_woods_plot_data, export_volcano_plot, export_volcano_plot_data, export_woods_plot, export_woods_plot_data, hydro_prop, kin_download_file_columns, kin_download_file_rows, kin_fractional, kin_log_x, kin_plot_title, kin_plot_title_size, kin_plot_x_label, kin_plot_x_label_size, kin_plot_y_label, kin_plot_y_label_size, kin_plot_y_range, kin_theory, kin_time_0, kin_time_100, kin_uncertainty, plot_range, plot_x_range, qc_state_first, qc_state_second, qc_time_0, qc_time_t, rep_plot_title, rep_plot_title_size, rep_plot_x_label, rep_plot_x_label_size, rep_plot_y_label, rep_plot_y_label_size, rep_state, rep_time, reset_peptide_list, sequence_length, sequence_start_shift, state_first, state_second, theory, time_0, time_100, time_t, vol_confidence_level, vol_interval, vol_p_adjustment_method, vol_state_1, vol_state_2, vol_timepoints, vol_x_range, vol_y_range, volcano_plot_title, volcano_plot_title_size, volcano_plot_x_label, volcano_plot_x_label_size, volcano_plot_y_label, volcano_plot_y_label_size, woods_plot_title, woods_plot_title_size, woods_plot_x_label, woods_plot_x_label_size, woods_plot_y_label, woods_plot_y_label_size, woods_plot_y_range", ", ")[[1]]

## The helper files must be in inst/HaDeX/docs localization

setwd("./inst/HaDeX/docs")

######################
## time_0 ############
######################

time_0_helper <- "## Minimal exchange control

Select minimal exchange control time of measurement. "
  
lapply(grep("time_0", app_inputs), function(i){

    write(time_0_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)

  })

######################
## time_t ############
######################

time_t_helper <- "## Chosen time point of measurement

Select time point of measurement that the calculations are done for. The result of this measurement is compared with minmal and maximal (if fractional values are selected) exchange control.
For more information, see the c."

lapply(grep("time_t", app_inputs), function(i){
  
  write(time_t_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## time_100 ##########
######################

time_100_helper <- "## Maximal exchange control

Select minimal exchange control time of measurement - if the time point is chosen, the values are chosen within each state. If `chosen control` is selected, the chosen maximal exchange control from the `Input data` settings is selected for all of the states.

Only for fractional values. "

lapply(grep("time_100", app_inputs), function(i){
  
  write(time_100_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## theory ############
######################

theory_helper <- "## Calculation types

### Theoretical

Measured deuterium uptake is compared with theoretical values - the minimal exchange control is mass of a peptide with only hydrogens, and the maximal exchange control is a mass of a peptide with only deuterons. The correction on D20 concentration is made, based on the value from `Input data` settings. 

Using theoretical values causes the experimental back-exchange information to be lost. The maximal exchange control is the maximal possible based on the sequence (all H exchanged to D), the information about conformation is lost.

### Experimental

Measured deuterium uptake is compared with experimenally measured minimal and maximal exchange control.  

### More information

To get more insight, please see the [documentation](https://hadexversum.github.io/HaDeX/articles/datafiles.html)."

lapply(grep("theory", app_inputs), function(i){
  
  write(theory_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## fractional ########
######################

fractional_helper <- ""

lapply(grep("fractional", app_inputs), function(i){
  
  write(fractional_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## state_1 ###########
######################

state_1_helper <- ""

lapply(grep("state_1", app_inputs), function(i){
  
  write(state_1_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## state_2 ###########
######################

state_2_helper <- ""

lapply(grep("state_2", app_inputs), function(i){
  
  write(state_2_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## timepoints ########
######################

timepoints_helper <- ""

lapply(grep("timepoints", app_inputs), function(i){
  
  write(timepoints_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## uncertainty #######
######################

uncertainty_helper <- ""

lapply(grep("uncertainty", app_inputs), function(i){
  
  write(uncertainty_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## title_size ########
######################

title_size_helper <- ""

lapply(grep("title_size", app_inputs), function(i){
  
  write(title_size_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## x_label_size ######
######################

x_label_size_helper <- ""

lapply(grep("x_label_size", app_inputs), function(i){
  
  write(x_label_size_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## y_label_size ###### 
######################

y_label_size_helper <- ""

lapply(grep("y_label_size", app_inputs), function(i){
  
  write(y_label_size_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## confidence_level ##
######################

confidence_level_helper <- ""

lapply(grep("confidence_level", app_inputs), function(i){
  
  write(confidence_level_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

######################
## show_uncertainty ##
######################

show_uncertainty_helper <- ""

lapply(grep("show_uncertainty", app_inputs), function(i){
  
  write(show_uncertainty_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})
