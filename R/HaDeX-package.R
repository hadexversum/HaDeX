#' HaDeX
#'
#' @description The \code{HaDeX} package is a toolbox for the analysis of HDX-MS data.
#'
#' @author Weronika Puchala, Michal Burdukiewicz.
#' @docType package
#' @name HaDeX-package
#' @aliases HaDeX
"_PACKAGE"

if(getRversion() >= "2.15.1")  utils::globalVariables(c('.', 'Center', 'End', 'Exposure', 'File', 'Inten', 'MHP', 'MaxUptake', 
                         'Med_Sequence', 'Protein', 'Sequence', 'Start', 'State', 'State_Exposure', 
                         'avg_exp_mass', 'avg_theo_in_time_1', 'avg_theo_in_time_2', 
                         'chosen_time_mean_1', 'chosen_time_mean_2', 'diff_frac_exch', 'diff_theo_frac_exch', 
                         'err_avg_theo_in_time_1', 'err_avg_theo_in_time_2', 'err_chosen_time_mean_1', 
                         'err_chosen_time_mean_2', 'err_diff_theo_frac_exch', 'err_frac_exch', 
                         'err_frac_exch_state_1', 'err_frac_exch_state_2', 'err_in_time_mean_1', 
                         'err_in_time_mean_2', 'err_out_time_mean_1', 'err_out_time_mean_2', 
                         'exp_mass', 'frac_exch_state_1', 'frac_exch_state_2', 
                         'in_time_mean_1', 'in_time_mean_2', 'out_time_mean_1', 'out_time_mean_2', 'z',
                         'abs_avg_theo_in_time_1', 'abs_avg_theo_in_time_2', 'abs_diff_frac_exch', 'abs_diff_theo_frac_exch', 
                         'abs_frac_exch_state_1', 'abs_frac_exch_state_2', 'colour', 'err_abs_avg_theo_in_time_1', 
                         'err_abs_avg_theo_in_time_2', 'err_abs_diff_frac_exch', 'err_abs_diff_theo_frac_exch', 
                         'err_abs_frac_exch_state_1', 'err_abs_frac_exch_state_2',
                         'Fragment', 'ID', 'Modification', 'abs_avg_theo_in_time', 'abs_frac_exch_state', 
                         'avg_theo_in_time', 'err_abs_avg_theo_in_time', 'err_abs_frac_exch_state', 
                         'err_avg_theo_in_time', 'err_frac_exch_state', 'err_time_chosen_mean', 
                         'err_time_in_mean', 'err_time_out_mean', 'frac_exch_state', 'time_chosen_mean', 
                         'time_in_mean', 'time_out_mean', 'value', 'x', 'coverage', 'time_chosen',
                         'prop', 'n_rep'))