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

app_inputs <- strsplit("butt_diff_confidence_level, butt_diff_fractional, butt_diff_show_test, butt_diff_state_1, butt_diff_state_2, butt_diff_theory, butt_diff_time_0, butt_diff_time_100, butt_diff_timepoints, butt_diff_uncertainty, butt_diff_x_range, butt_diff_y_range, butt_fractional, butt_state, butt_theory, butt_time_0, butt_time_100, butt_timepoints, butt_uncertainty, butt_x_range, butt_y_range, butterflyDifferential_plot_title, butterflyDifferential_plot_title_size, butterflyDifferential_plot_x_label, butterflyDifferential_plot_x_label_size, butterflyDifferential_plot_y_label, butterflyDifferential_plot_y_label_size, butterfly_plot_title, butterfly_plot_title_size, butterfly_plot_x_label, butterfly_plot_x_label_size, butterfly_plot_y_label, butterfly_plot_y_label_size, chic_diff_fractional, chic_diff_show_uncertainty, chic_diff_state_1, chic_diff_state_2, chic_diff_theory, chic_diff_time_0, chic_diff_time_100, chic_diff_timepoints, chic_diff_x_range, chic_fractional, chic_show_uncertainty, chic_state, chic_theory, chic_time_0, chic_time_100, chic_timepoints, chic_x_range, chicletDifferential_plot_title, chicletDifferential_plot_title_size, chicletDifferential_plot_x_label, chicletDifferential_plot_x_label_size, chicletDifferential_plot_y_label, chicletDifferential_plot_y_label_size, chiclet_plot_title, chiclet_plot_title_size, chiclet_plot_x_label, chiclet_plot_x_label_size, chiclet_plot_y_label, chiclet_plot_y_label_size, chosen_control, chosen_protein, chosen_state, comp_fractional, comp_plot_y_range, compare_states, comparison_plot_title, comparison_plot_title_size, comparison_plot_x_label, comparison_plot_x_label_size, comparison_plot_y_label, comparison_plot_y_label_size, confidence_limit, confidence_limit_2, data_file, deut_part, diff_state_1, diff_state_2, exam_apply_changes, exam_confidence, exam_protein_name, exam_state_name, examiner_fd_timepoint, export_butterfly_differential_plot, export_butterfly_differential_plot_data, export_butterfly_plot, export_butterfly_plot_data, export_chiclet_differential_plot, export_chiclet_differential_plot_data, export_chiclet_plot, export_chiclet_plot_data, export_comparison_plot, export_comparison_plot_data, export_kin_plot, export_kin_plot_data, export_overlap_dist, export_overlap_dist_data, export_overlap_graph, export_overlap_graph_data, export_quality_control_plot, export_quality_control_plot_data, export_theo_comparison_plot, export_theo_comparison_plot_data, export_theo_kin_plot, export_theo_kin_plot_data, export_theo_woods_plot, export_theo_woods_plot_data, export_volcano_plot, export_volcano_plot_data, export_woods_plot, export_woods_plot_data, hydro_prop, kin_download_file_columns, kin_download_file_rows, kin_fractional, kin_log_x, kin_plot_title, kin_plot_title_size, kin_plot_x_label, kin_plot_x_label_size, kin_plot_y_label, kin_plot_y_label_size, kin_plot_y_range, kin_theory, kin_time_0, kin_time_100, kin_uncertainty, plot_range, plot_x_range, qc_state_1, qc_state_2, qc_time_0, qc_time_t, rep_plot_title, rep_plot_title_size, rep_plot_x_label, rep_plot_x_label_size, rep_plot_y_label, rep_plot_y_label_size, rep_state, rep_time, reset_peptide_list, sequence_length, sequence_start_shift, theory, time_0, time_100, time_t, vol_confidence_level, vol_interval, vol_p_adjustment_method, vol_state_1, vol_state_2, vol_timepoints, vol_x_range, vol_y_range, volcano_plot_title, volcano_plot_title_size, volcano_plot_x_label, volcano_plot_x_label_size, volcano_plot_y_label, volcano_plot_y_label_size, woods_plot_title, woods_plot_title_size, woods_plot_x_label, woods_plot_x_label_size, woods_plot_y_label, woods_plot_y_label_size, woods_plot_y_range", ", ")[[1]]

## The helper files must be in inst/HaDeX/docs localization

setwd("./inst/HaDeX/docs")

####################################
######### MULTIPLE HELPERS #########
####################################

#############################
## time_0 ###################
#############################

time_0_helper <- "## Minimal exchange control

Minimal exchange control time of measurement. "
  
lapply(grep("time_0", app_inputs), function(i){

    write(time_0_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)

  })

#############################
## time_t ###################
#############################

time_t_helper <- "## Chosen time point of measurement

Time point of measurement that the calculations are done for. The result of this measurement is compared with minimal and maximal (if fractional values are selected) exchange control.
"

lapply(grep("time_t", app_inputs), function(i){
  
  write(time_t_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## time_100 #################
#############################

time_100_helper <- "## Maximal exchange control

Select minimal exchange control time of measurement - if the time point is chosen, the values are chosen within each state. If `chosen control` is selected, the chosen maximal exchange control from the `Input data` settings is selected for all of the states.

Only for fractional values. "

lapply(grep("time_100", app_inputs), function(i){
  
  write(time_100_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## theory ###################
#############################

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

#############################
## fractional ###############
#############################

fractional_helper <- "## Presented values

There are two forms to present values: fractional [%] and absolute [Da]. 
"

lapply(grep("fractional", app_inputs), function(i){
  
  write(fractional_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## state_1 ##################
#############################

state_1_helper <- "## State 1

Differential plots present the deuterium uptake difference between State 1 and State 2 in given time points."

lapply(grep("state_1", app_inputs), function(i){
  
  write(state_1_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## state_2 ##################
#############################

state_2_helper <- "## State 2

Differential plots present the deuterium uptake difference between State 1 and State 2 in given time points."

lapply(grep("state_2", app_inputs), function(i){
  
  write(state_2_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## timepoints ###############
#############################

timepoints_helper <- "## Chosen time points

The measurement is done for multiple time points. The plot can present all of them, or only selected by the user."

lapply(grep("timepoints", app_inputs), function(i){
  
  write(timepoints_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## uncertainty ##############
#############################

uncertainty_helper <- "## Uncertainty forms

There are three method to visualize the uncertainty: 

* ribbon: the points are connected with a ribbon indicating the uncerainty value,
* bars: each point is accompanied by the error bar,
* bars + line: each point is accompanied by the error bar and points are connected by the line (linewidth has no interpretation)."


lapply(setdiff(grep("uncertainty", app_inputs) , grep("show_uncertainty", app_inputs)), function(i){
  
  write(uncertainty_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## title_size ###############
#############################

title_size_helper <- "## Title size

Font size of the title of the plot can be changed according to the user's preferences. 
Sizes below 10 are not advised due to weak readability."

lapply(grep("title_size", app_inputs), function(i){
  
  write(title_size_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## x_label_size #############
#############################

x_label_size_helper <- "## Label size

Font size of the X label of the plot can be changed according to the user's preferences. 
Sizes below 10 are not advised due to weak readability.

The text on the axis have the same size as the axis label. 

The legend has the same size as the X axis label."

lapply(grep("x_label_size", app_inputs), function(i){
  
  write(x_label_size_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## y_label_size #############
#############################

y_label_size_helper <- "## Label size

Font size of the Y label of the plot can be changed according to the user's preferences. 
Sizes below 10 are not advised due to weak readability."

lapply(grep("y_label_size", app_inputs), function(i){
  
  write(y_label_size_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## show_uncertainty #########
#############################

show_uncertainty_helper <- "## Uncertainty indicator

On this type of plot, the uncertainty may reduce the readibility. There is a possibility to hide the uncertainty of the measurement (but not advised). "

lapply(grep("show_uncertainty", app_inputs), function(i){
  
  write(show_uncertainty_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## confidence_level #########
#############################

confidence_level_helper <- ""

lapply(grep("confidence_level", app_inputs), function(i){
  
  write(confidence_level_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## state ####################
#############################

state_helper <- ""

lapply(grep("state$", app_inputs), function(i){
  
  write(state_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})


####################################
########## SINGLE HELPERS ##########
####################################


#############################
## show_test ################
#############################

show_test_helper <- ""

lapply(grep("show_test", app_inputs), function(i){
  
  write(show_test_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})


#############################
## deut_part ################
#############################

deut_part_helper <- ""

lapply(grep("deut_part", app_inputs), function(i){
  
  write(deut_part_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## rep_time #################
#############################

rep_time_helper <- ""

lapply(grep("rep_time", app_inputs), function(i){
  
  write(rep_time_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})


###############################
## kin_download_file_columns ##
###############################

kin_download_file_columns_helper <- ""

lapply(grep("kin_download_file_columns", app_inputs), function(i){
  
  write(kin_download_file_columns_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})


#############################
## kin_download_file_rows ###
#############################

kin_download_file_rows_helper <- ""

lapply(grep("kin_download_file_rows", app_inputs), function(i){
  
  write(kin_download_file_rows_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})


#############################
## vol_interval #############
#############################

vol_interval_helper <- ""

lapply(grep("vol_interval", app_inputs), function(i){
  
  write(vol_interval_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})


#############################
## kin_log_x ################
#############################

kin_log_x_helper <- ""

lapply(grep("kin_log_x", app_inputs), function(i){
  
  write(kin_log_x_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})


#############################
## vol_p_adjustment_method ##
#############################

vol_p_adjustment_method_helper <- ""

lapply(grep("vol_p_adjustment_method", app_inputs), function(i){
  
  write(vol_p_adjustment_method_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

####################################
############# MISSING ##############
####################################

library(stringr)

already_done <- str_remove(list.files(), ".md")
excluded <- app_inputs[c(grep("export", app_inputs), grep("range", app_inputs), grep("title$", app_inputs), grep("y_label$", app_inputs), grep("x_label$", app_inputs))]
excluded2 <- c("reset_peptide_list", "hydro_prop",  "data_file", "exam_apply_changes")

setdiff(app_inputs, c(already_done, excluded, excluded2))
