validate(need(!(input[["state_first"]] == input[["state_second"]]), "Please select two different states."))

if (input[["theory"]]) {
  
  if (input[["calc_type"]] == "relative") {
    
    wp <- differential_plot_theo() 
    
  } else {
    
    wp <- differential_plot_theo_abs() 
    
  }
  
} else {
  
  if (input[["calc_type"]] == "relative") {
    
    wp <- differential_plot_exp() 
    
  } else {
    
    wp <- differential_plot_exp_abs() 
    
  }
  
}

wp <- wp + coord_cartesian(xlim = c(input[["plot_x_range"]][[1]], input[["plot_x_range"]][[2]]),
                           ylim = c(input[["woods_plot_y_range"]][[1]], input[["woods_plot_y_range"]][[2]])) +
  labs(title = input[["woods_plot_title"]],
       x = input[["woods_plot_x_label"]],
       y = input[["woods_plot_y_label"]])
