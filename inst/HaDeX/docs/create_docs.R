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

app_inputs1 <- c("butt_diff_confidence_level", "butt_diff_fractional", "butt_diff_p_adjustment_method", "butt_diff_show_houde", "butt_diff_show_tstud", "butt_diff_state_1", "butt_diff_state_2", "butt_diff_theory", "butt_diff_time_0", "butt_diff_time_100", "butt_diff_timepoints", "butt_diff_uncertainty", "butt_diff_x_range", "butt_diff_y_range", "butt_fractional", "butt_state", "butt_theory", "butt_time_0", "butt_time_100", "butt_timepoints", "butt_uncertainty", "butt_x_range", "butt_y_range", "butterflyDifferential_plot_title", "butterflyDifferential_plot_title_size", "butterflyDifferential_plot_x_label", "butterflyDifferential_plot_x_label_size", "butterflyDifferential_plot_y_label", "butterflyDifferential_plot_y_label_size", "butterfly_plot_title", "butterfly_plot_title_size", "butterfly_plot_x_label", "butterfly_plot_x_label_size", "butterfly_plot_y_label", "butterfly_plot_y_label_size", "chic_confidence_level", "chic_diff_fractional", "chic_diff_p_adjustment_method", "chic_diff_show_houde", "chic_diff_show_tstud", "chic_diff_show_uncertainty", "chic_diff_state_1", "chic_diff_state_2", "chic_diff_theory", "chic_diff_time_0", "chic_diff_time_100", "chic_diff_timepoints", "chic_diff_x_range", "chic_fractional", "chic_show_uncertainty", "chic_state", "chic_theory", "chic_time_0", "chic_time_100", "chic_timepoints", "chic_x_range", "chicletDifferential_plot_title", "chicletDifferential_plot_title_size", "chicletDifferential_plot_x_label", "chicletDifferential_plot_x_label_size", "chicletDifferential_plot_y_label", "chicletDifferential_plot_y_label_size", "chiclet_plot_title", "chiclet_plot_title_size", "chiclet_plot_x_label", "chiclet_plot_x_label_size", "chiclet_plot_y_label", "chiclet_plot_y_label_size", "chosen_control", "chosen_protein", "chosen_state", "comp_fractional", "comp_plot_y_range", "compare_states", "comparison_plot_title", "comparison_plot_title_size", "comparison_plot_x_label", "comparison_plot_x_label_size", "comparison_plot_y_label", "comparison_plot_y_label_size", "confidence_level", "data_file", "deut_part", "diff_comp_times_t", "diff_hide_insignificant", "diff_kin_confidence_level", "diff_kin_download_file_columns", "diff_kin_download_file_rows", "diff_kin_fractional", "diff_kin_log_x", "diff_kin_p_adjustment_method", "diff_kin_plot_title", "diff_kin_plot_title_size", "diff_kin_plot_x_label", "diff_kin_plot_x_label_size", "diff_kin_plot_y_label", "diff_kin_plot_y_label_size", "diff_kin_plot_y_range", "diff_kin_show_houde", "diff_kin_show_tstud", "diff_kin_state_1", "diff_kin_state_2", "diff_kin_theory", "diff_kin_time_0", "diff_kin_time_100", "diff_kin_uncertainty", "diff_p_adjustment_method", "diff_show_houde", "diff_show_tstud", "diff_state_1", "diff_state_2", "exam_apply_changes", "exam_confidence", "exam_protein_name", "exam_state_name", "examiner_fd_timepoint", "export_all_data", "export_all_plots", "export_butterfly_differential_plot", "export_butterfly_differential_plot_data", "export_butterfly_plot", "export_butterfly_plot_data", "export_chiclet_differential_plot", "export_chiclet_differential_plot_data", "export_chiclet_plot", "export_chiclet_plot_data", "export_comparison_plot", "export_comparison_plot_data", "export_kin_plot", "export_kin_plot_data", "export_overlap_dist", "export_overlap_dist_data", "export_overlap_graph", "export_overlap_graph_data", "export_quality_control_plot", "export_quality_control_plot_data", "export_replicate_histograms", "export_replicate_histograms_data", "export_replicate_plots", "export_replicate_plots_data", "export_volcano_plot", "export_volcano_plot_data", "export_woods_plot", "export_woods_plot_data", "hydro_prop", "kin_download_file_columns", "kin_download_file_rows", "kin_fractional", "kin_log_x", "kin_plot_title", "kin_plot_title_size", "kin_plot_x_label", "kin_plot_x_label_size", "kin_plot_y_label", "kin_plot_y_label_size")

app_inputs2 <- c("kin_plot_y_range", "kin_theory", "kin_time_0", "kin_time_100", "kin_uncertainty", "man_confidence_level", "man_p_adjustment_method", "man_separate_times", "man_show_position", "man_state_1", "man_state_2", "man_times", "mass_uptake_log_x", "mass_uptake_plot_title", "mass_uptake_plot_title_size", "mass_uptake_plot_x_label", "mass_uptake_plot_x_label_size", "mass_uptake_plot_y_label", "mass_uptake_plot_y_label_size", "mass_uptake_plot_y_range", "measures_plot_title", "measures_plot_title_size", "measures_plot_x_label", "measures_plot_x_label_size", "measures_plot_y_label", "measures_plot_y_label_size", "measures_show_charge", "measures_state", "measures_time", "no_deut_control", "plot_range", "plot_x_range", "qc_state_1", "qc_state_2", "qc_time_0", "qc_time_t", "rep_state", "rep_time", "reset_peptide_list", "sequence_length", "sequence_start_shift", "theory", "time_0", "time_100", "time_t", "un_aggregated", "un_separate_times", "un_state", "un_timepoints", "vol_color_times", "vol_confidence_level", "vol_fractional", "vol_hide_insignificant", "vol_interval", "vol_p_adjustment_method", "vol_sequence_range", "vol_show_insignificant_grey", "vol_state_1", "vol_state_2", "vol_time_0", "vol_time_100", "vol_timepoints", "vol_x_range", "vol_y_range", "volcano_plot_title", "volcano_plot_title_size", "volcano_plot_x_label", "volcano_plot_x_label_size", "volcano_plot_y_label", "volcano_plot_y_label_size", "woods_plot_title", "woods_plot_title_size", "woods_plot_x_label", "woods_plot_x_label_size", "woods_plot_y_label", "woods_plot_y_label_size", "woods_plot_y_range")

app_inputs <- c(app_inputs1, app_inputs2)

## The helper files must be in inst/HaDeX/docs localization

setwd("./inst/HaDeX/docs")

####################################
############# MISSING ##############
####################################

library(stringr)

already_done <- str_remove(list.files(), ".md")
excluded <- app_inputs[c(grep("export", app_inputs), grep("range", app_inputs), grep("title$", app_inputs), grep("y_label$", app_inputs), grep("x_label$", app_inputs))]
excluded2 <- c("reset_peptide_list", "hydro_prop",  "data_file", "exam_apply_changes")

setdiff(app_inputs, c(already_done, excluded, excluded2))

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

confidence_level_helper <- "## Confidence level

The confidence intervals are calculated based on confidence level."

lapply(grep("confidence_level", app_inputs), function(i){
  
  write(confidence_level_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## state ####################
#############################

state_helper <- "## Biological state

Biological state of chosen protein of interest."

lapply(grep("state$", app_inputs), function(i){
  
  write(state_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})




####################################
########## SINGLE HELPERS ##########
####################################

#############################
## show_test ################
#############################

show_test_helper <- "## Test indicator

Confidence intervals on differential butterfly plot can affect readibility. They can be shown or not, as the user prefers.

The confidence intervals are calculated based on Houde test. For more information see the documentation.

Confidence intervals indicate if the measurement is statistically significant. If the result of the measurement is above 
confidence interval on 98% confidence level, it means that for 98% the null hypothesis (that there is no difference between two states) can be rejected.
"

lapply(grep("show_test", app_inputs), function(i){
  
  write(show_test_helper, file = paste0(app_inputs[i], ".md"), append = FALSE)
  
})

#############################
## deut_part ################
#############################

deut_part_helper <- "## Deuterium concentration

Deuterium concentration indicates how much deuterium is in the buffer the protein is incubated in. This value is used when result of measurement is compared with theoretical exchange controls. For more information see the documentation."

write(deut_part_helper, file = "deut_part.md", append = FALSE)

#############################
## rep_time #################
#############################

rep_time_helper <- "## Time point

Time point of the measurement of interest."

write(rep_time_helper, file = "rep_time.md", append = FALSE)

###############################
## kin_download_file_columns ##
###############################

kin_download_file_columns_helper <- "## Number of columns in file

The deuterium uptake plots are arranged on the sheets of multi page pdf document. The number of columns can be adjusted. 

The number of columns on one page should not be bigger than 4, as it may affect the readibility. 
"

write(kin_download_file_columns_helper, file = "kin_download_file_columns.md", append = FALSE)

#############################
## kin_download_file_rows ###
#############################

kin_download_file_rows_helper <- "## Number of columns in file

The deuterium uptake plots are arranged on the sheets of multi page pdf document. The number of rows can be adjusted. 

The number of rows on one page should not be bigger than 4, as it may affect the readibility. 
"

write(kin_download_file_rows_helper, file = "kin_download_file_rows.md", append = FALSE)

#############################
## vol_interval #############
#############################

vol_interval_helper <- "## Confidence calculations

The confidence limits can be calculated based on whole dataset of measurements, or only the ones visible on the plot (selected on the left)."

write(vol_interval_helper, file = "vol_interval.md", append = FALSE)

#############################
## kin_log_x ################
#############################

kin_log_x_helper <- "## Logarithm indicator

The time point of measurement are on a wide scale - from seconds to hours. The logarythmic values on the time scale help to distinquish the values and can increase readibility."

write(kin_log_x_helper, file =  "kin_log_x.md", append = FALSE)

#############################
## vol_p_adjustment_method ##
#############################

vol_p_adjustment_method_helper <- "## Adjustment indicator

The P-values calculated for differential deuterium uptake can be adjusted using Benjamini & Hochberg (BH) or Bonferroni correction, if needed. "

write(vol_p_adjustment_method_helper, file = "vol_p_adjustment_method.md", append = FALSE)

#############################
## chosen_control ###########
#############################

chosen_control_helper <- '## Maximal exchange control

Selected measurement is treated as maximal exchange control for all biological states of given protein. '

write(chosen_control_helper, file = 'chosen_control.md', append = FALSE)

#############################
## chosen_protein ###########
#############################

chosen_protein_helper <- '## Protein of interest

HaDeX accepts files with more than one protein, but all the calculations are done for the chosen protein. The name of the chosen protein is preserved in all of the tabs and all of the plot titles. '

write(chosen_protein_helper, file = 'chosen_protein.md', append = FALSE)

#############################
## compare_states ###########
#############################

compare_states_helper <- '## States comparison

Chosen states appear on the comparison plot on the right.'

write(compare_states_helper, file = 'compare_states.md', append = FALSE)

#############################
## exam_confidence ##########
#############################

exam_confidence_helper <- '## Measurement confidence

Files from HDeXaminer have a `Confidence` column that tells how
reliable each measurement (and recognition of the peptide) is.
There are three levels of confidence: High, Medium, and Low.

The user can choose which results should be considered for processing. Following the authors of HDeXaminer, we recommend
to ignore low-confidence results.'

write(exam_confidence_helper, file = 'exam_confidence.md', append = FALSE)

#############################
## exam_protein_name ########
#############################

exam_protein_name_helper <- '## Protein name

Protein name is not provided in the file from HDeXaminer, 
so it is artificially obtained from `Protein State` value.
The user can change this label to a more descriptive one.'

write(exam_protein_name_helper, file = 'exam_protein_name.md', append = FALSE)

#############################
## exam_state_name ##########
#############################

exam_state_name_helper <- '## State names

The user can change the state labels obtained from the file.
For this operation to be successful, some requirements should
be taken in the account:
* the same number of labels as obtained from the file should
be provided,
* the labels should be separated by comma (without spaces).'

write(exam_state_name_helper, file = 'exam_state_name.md', append = FALSE)

#############################
## examiner_fd_timepoint ####
#############################

examiner_fd_timepoint_helper <- '## FD time point

Files from HDeXaminer don\'t provide the numerical value of time point 
for the fully deuterated sample but flag it as "FD". For precise values 
on the plots and data, HaDeX needs a numerical value in minutes.
The value (1440 min = 24 h) is commonly used and is suggested, but 
the user can change it to its real value.'

write(examiner_fd_timepoint_helper, file = 'examiner_fd_timepoint.md', append = FALSE)

#############################
## sequence_length ##########
#############################

sequence_length_helper <- '## Sequence C-terminus

If the C-terminus of a sequence is trimmed, provide its true position. Imputed amino acids are represented as "x" in the reconstructed sequence.'

write(sequence_length_helper, file = 'sequence_length.md', append = FALSE)

#############################
## sequence_start_shift #####
#############################

sequence_start_shift_helper <- '## Sequence N-terminus

If the N-terminus of a sequence is trimmed, provide its true position. Imputed amino acids are represented as "x" in the reconstructed sequence.'

write(sequence_start_shift_helper, file = 'sequence_start_shift.md', append = FALSE)

####################################
############# OUTPUTS ##############
####################################

##################################
##  ################
##################################



##################################
## stateOverlap ##################
##################################

stateOverlap_helper <- '

## Protein coverage

This plot presents the peptide pool for selected protein. Each line shows a peptide, with its length and position on the protein sequence.

'
write(stateOverlap_helper, file = "stateOverlap.md", append = FALSE)

##################################
## stateOverlapDist ##############
##################################

stateOverlapDist_helper <- '

## Position frequency

This histogram shows how many peptides cover each position in the protein structure.
'
write(stateOverlapDist_helper, file = "stateOverlapDist.md", append = FALSE)



##################################
## adjust_colors #################
##################################

adjust_colors_helper <- '

## Select state colors

There is a possibility to select colors for biological state for the comparison plot.
Under this button, there are fields for color codes for each state. The accepted format is Hex color codes.

For exploring the colors, we recommend [color picker](https://htmlcolorcodes.com/) or [palette generator](https://coolors.co/).

'

write(adjust_colors_helper, file = "adjust_colors.md", append = FALSE)

##################################
## replicatesPlot ################
##################################

replicates_plot_helper <- '

## Replicates - mass

This plot presents the values of masured mass (in daltons) from replicates for selected peptide.

The dotted line indicates the average measured mass from the replicates - this value is considered as result of measurement for selected peptide.

Each replicate is specified by the `File` value, chosen by the exprimenter. 

This plot shows the data for selected time point of the measurement. 
'

write(replicates_plot_helper, file = 'replicatesPlot.md', append = FALSE)

##################################
## replicates_charge_plot ########
##################################

replicates_charge_plot_helper <- ' 

## Replicates - charges

This plot represents the measured charge values for selected peptide for each replicate. 

Each replicate is specified by the `File` value, chosen by the exprimenter. 

This plot shows the data for selected time point of the measurement. 
'

write(replicates_charge_plot_helper, file = 'replicatesChargePlot.md', append = FALSE)

##################################
## replicates_histogram ##########
##################################

replicates_histogram_helper <- '

## Number of replicates

The histogram shows how many replicates were done for specific peptide in selected time point of the measurement. The peptides are specified by their ID. 

`Data` tab and the tooltips provide additional information e.g. the position of the peptide in the protein sequence. 
'

write(replicates_histogram_helper, file = 'replicatesHistogram.md', append = FALSE)

##################################
## all_replicates_histogram ######
##################################

all_replicates_histogram_helper <- '

## Number of replicates per peptide

The histogram shows how many replicates were done for specific peptide during the experiment. The colors indicates the time point of measurement. 
The peptides are specified by their ID. 

`Data` tab and the tooltips provide additional information e.g. the position of the peptide in the protein sequence. 
'

write(all_replicates_histogram_helper, file = 'allReplicatesHistogram.md', append = FALSE)

##################################
## timesReplicatesHistogram ######
##################################

timesReplicatesHistogram_helper <- '

## Number of replicates per time point

The histogram shows how many replicates were done for specific time point of measurement during the experiment. The color indicates the ID of each peptide, as it 
is cumulatative value. 
'

write(timesReplicatesHistogram_helper, file = "timesReplicatesHistogram.md", append = FALSE)

##################################
## peptide_list_data #############
##################################

peptide_list_data_helper <- '

## Peptide list

This table presents available peptides for selected protein.

Uptake curve for selected peptides is plotted on the right panel. Multiple peptides can be selected but we recommed selecting no more than four peptides in order to keep the plot readable.
'

write(peptide_list_data_helper, file = 'peptide_list_data.md', append = FALSE)

##################################
## kinetic_plot_chosen_peptides ##
##################################

kinetic_plot_chosen_peptides_helper <- '

## Uptake curve 

This plot presents uptake curves of the selected peptides. 

On the X axis there are time points of measurement (in minutes). The axis is in logaritmic scale for readability (this can be changed in the settings on the left panel). The minimal uptake control time point of the measurement is not shown on the plot.

On th Y axis there are values of deuterium uptake, in selected form.
'

write(kinetic_plot_chosen_peptides_helper, file = 'kinetic_plot_chosen_peptides.md', append = FALSE)

##################################
## quality_control_plot ##########
##################################

quality_control_plot_helper <- '

## Quality control

This plot shows the change in the uncertainty of deuteration levels as a function of maximal exchange control. It considers only experimental fractional values, where maximal exchange control is used. It presents the efect that selected maximal exchage control has on the data.

On the X axis there are time points of measurement that are used as maximal exchange control. The axis is in logaritmic scale due to the readability of the plot. 

On the Y axis there is averaged uncertainty for all the peptides for the time points of measurement smaller that the maximal exchange control. 

If the experiment is performed correctly, the values of the plot should be smaller with the increase of incubation time for maximal exchange control.
'

write(quality_control_plot_helper, file = 'quality_control_plot.md', append = FALSE)

##################################
## rep_sequence ##################
##################################

rep_sequence_helper <- '

## Peptide list

This table presents available peptides for selected protein, state and time point of measurement. 

Data for selected peptide is plotted on the right panel. Only one peptide can be selected.
'

write(rep_sequence_helper, file = "rep_sequence.md", append = FALSE)

##################################
## chicletDifferentialPlot #######
##################################

chicletDifferentialPlot_helper <- "## Chiclet Differential Plot

Chiclet differential plot shows the deuterium uptake difference between two biological states in the form of a heatmap. One tile indicates the peptide (identified by its ID - number arranged by the start position) in a time point of measurement. The color of the tile indicates the deuterium uptake difference (according to the legend below the plot).

For more information see the [documentation](https://hadexversum.github.io/HaDeX/articles/visualization.html#chiclet-differential-1).

"

write(chicletDifferentialPlot_helper, file = "chicletDifferentialPlot.md", append = FALSE)

##################################
## chicletPlot ###################
##################################

chicletPlot_helper <- "## Chiclet Plot

Chiclet plot shows the fractional deuterium uptake in the form of a heatmap for the peptides in a given biological state. One tile indicates the peptide (identified by its ID - number arranged by the start position) in a time point of measurement. The color of the tile indicates the fractinal deuterium uptake (according to the legend below the plot).

For more information see the [documentation](https://hadexversum.github.io/HaDeX/articles/visualization.html#chiclet-plot-1).
"

write(chicletPlot_helper, file = "chicletPlot.md", append = FALSE)

##################################
## volcanoPlot ###################
##################################

volcanoPlot_helper <- "## Volcano Plot

The volcano plot shows the deuterium uptake difference for two biological states for peptide and its p-value for double testing on statistical significance (Weis et al.). On the x-axis, there is a deuterium uptake difference with its uncertainty (combined and propagated). On the y-axis, there is a P-value calculated for each peptide in a specifc time point of a measurement as a un-paired t-test on given significance level (on mass measurement from the replicates to indicate if the measured mean is significantly different between two states, as the deuterium uptake difference between states can be rewritten as

$$Delta D=D_A−D_B=m_{t,A}−m_0−(m_{t,B}−m_0)=m_{t,A}−m_{t,B}$$


for states A and B. The values of deuterium uptake difference from all time points are shown on the plot.

The dotted red lines indicate confidence limits for the values. The horizontal line indicates the confidence limit based on chosen confidence level to give a threshold on a P-value. The vertical lines indicate the confidence limit from Houde test for all time points and indicate a threshold on deuterium uptake difference. The statistically significant points are in the top left and right corners of the plot.

For more information see the [documentation](https://hadexversum.github.io/HaDeX/articles/visualization.html#volcano-plot-1).
"

write(volcanoPlot_helper, file = "volcanoPlot.md", append = FALSE)

##################################
## butterflyDifferentialPlot #####
##################################

butterflyDifferentialPlot_helper <- "## Butterfly Differential Plot

Butterfly differential plot shows the deuterium uptake difference between two biological states in the form of a butterfly plot. It shows the results for a peptide ID (peptides are numbered arranged by the start position). The results are shown for different time points at once (time points of measurement are indicated by the color).

For more information see the [documentation](https://hadexversum.github.io/HaDeX/articles/visualization.html#butterfly-differential-plot-1).
"

write(butterflyDifferentialPlot_helper, file = "butterflyDifferentialPlot.md", append = FALSE)

##################################
## comparisonPlot ################
##################################

comparisonPlot_helper <- "## Comparison Plot

The comparison plot shows deuterium uptake of the peptides in a given time point, with information on the length of the peptide and its position in the protein sequence. It allows comparing the results of different biological states.

For more information see the [documentation](https://hadexversum.github.io/HaDeX/articles/visualization.html#comparison-plot-1)."

write(comparisonPlot_helper, file = "comparisonPlot.md", append = FALSE)

##################################
## differentialPlot ##############
##################################

differentialPlot_helper <- "## Woods Differential Plot

Woods plot presents the deuterium uptake difference between two biological states for the peptides. The results are presented with respect to the length of the peptide and its position in the protein sequence for a given time point of the measurement. The statistical test (Houde et al.), the confidence level is calculated based on the measurements in chosen time point of the measurement) is applied to determine the confidence limits values at the chosen level.

For more information see the [documentation](https://hadexversum.github.io/HaDeX/articles/visualization.html#woods-plot-1).
"

write(differentialPlot_helper, file = "differentialPlot.md", append = FALSE)

##################################
## butterflyPlot #################
##################################

butterflyPlot_helper <- "## Butterfly Plot

Butterfly plot presents the deuterium uptake for all peptides in a given state at different time points at once. Each time point of measurement is indicated by a different color. Peptides are identified by their ID (peptides are numbered arranged by the start position).

For more information see the [documentation](https://hadexversum.github.io/HaDeX/articles/visualization.html#butterfly-plot-1).
"

write(butterflyPlot_helper, file = "butterflyPlot.md", append = FALSE)

