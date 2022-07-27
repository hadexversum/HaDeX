#' Coverage heatmap
#' 
#' @description Coverage heatmap with color indicating
#' specific value
#' 
#' @param x_dat data created using calculate_ or create_ 
#' function
#' @param protein selected protein
#' @param value value to be presented
#' @param time_t ...
#' 
#' @details ...
#' 
#' @return a \code{\link{ggplot}} object
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{plot_coverage}}
#' 
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' uptake_dat <- create_uptake_dataset(dat, states = "CD160")
#' plot_coverage_heatmap(uptake_dat)
#' plot_coverage_heatmap(uptake_dat, value = "frac_deut_uptake", time_t = 0.167)
#' plot_coverage_heatmap(uptake_dat, value = "err_frac_deut_uptake", time_t = 0.167)
#'
#' diff_uptake_dat <- create_diff_uptake_dataset(dat)
#' plot_coverage_heatmap(diff_uptake_dat)
#' plot_coverage_heatmap(diff_uptake_dat, value = "diff_frac_deut_uptake")
#' plot_coverage_heatmap(diff_uptake_dat, value = "diff_frac_deut_uptake", time_t = 0.167)
#' plot_coverage_heatmap(diff_uptake_dat, value = "err_diff_frac_deut_uptake", time_t = 0.167)
#' 
#' auc_dat <- calculate_auc(create_uptake_dataset(dat))
#' plot_coverage_heatmap(auc_dat, value = "auc")
#' 
#' bex_dat <- calculate_back_exchange(dat, state = "CD160")
#' plot_coverage_heatmap(bex_dat, value = "back_exchange")
#' 
#' @export plot_coverage_heatmap

plot_coverage_heatmap <- function(x_dat,
                                  protein = x_dat[["Protein"]][1],
                                  state = NULL, 
                                  value = NULL,
                                  time_t = NULL){
  
  if(is.null(value) || !(value %in% colnames(x_dat))) {
    return(plot_coverage(x_dat, protein = protein, states = state, show_blanks = F))
  }
  
  x_dat <- as.data.table(x_dat)
  x_dat <- x_dat[Protein == protein, ]
  
  if(value!="auc" & value!='back_exchange'){
    time_t <- fcoalesce(c(time_t, attr(x_dat, "time_t")))[1]
    if(is.null(time_t)) {
      message("No time point selected!")
      return(plot_coverage(x_dat, protein = protein, states = state, show_blanks = F))
    } else {
      x_dat <- x_dat[Exposure == time_t, ]
    }
  }
  
  ## levels
  levels <- rep(NA, (nrow(x_dat)))
  levels[1] <- 1
  start <- x_dat[["Start"]]
  end <- x_dat[["End"]]
  for(i in 1:(nrow(x_dat) - 1)) {
    for(level in 1:max(levels, na.rm = TRUE)) {
      if(all(start[i + 1] > end[1:i][levels == level] | end[i + 1] < start[1:i][levels == level], na.rm = TRUE)) {
        levels[i + 1] <- level
        break
      } else {
        if(level == max(levels, na.rm = TRUE)) {
          levels[i + 1] <- max(levels, na.rm = TRUE) + 1
        } 
      }
    }
  }
  x_dat[, ID := levels]
  ## end of levels
  
  value_label <- fcase(value == "frac_deut_uptake", "Frac DU [%]",
                       value == "deut_uptake", "DU [Da]",
                       value == "theo_frac_deut_uptake", "Theo Frac DU [%]",
                       value == "theo_deut_uptake", "Theo DU [Da]",
                       value == "diff_frac_deut_uptake", "Diff Frac DU [%]",
                       value == "diff_deut_uptake", "Diff DU [Da]",
                       value == "diff_theo_frac_deut_uptake", "Diff Theo Frac DU [%]",
                       value == "diff_theo_deut_uptake", "Diff Theo DU [Da]",
                       value == "err_frac_deut_uptake", "Err(Frac DU) [%]",
                       value == "err_deut_uptake", "Err(DU) [Da]",
                       value == "err_theo_frac_deut_uptake", "Err(Theo Frac DU) [%]",
                       value == "err_theo_deut_uptake", "Err(Theo DU) [Da]",
                       value == "err_diff_frac_deut_uptake", "Err(Diff Frac DU) [%]",
                       value == "err_diff_deut_uptake", "Err(Diff DU) [Da]",
                       value == "err_diff_theo_frac_deut_uptake", "Err(Diff Theo Frac DU) [%]",
                       value == "err_diff_theo_deut_uptake", "Err(Diff Theo DU) [Da]",
                       value == "auc", "AUC",
                       value == "back_exchange", "Back Exchange",
                       TRUE, "")
  
  if(is.null(time_t)) { 
    title <-  "Peptide coverage heatmap"
    } else { title <- paste0("Peptide coverage heatmap in ", time_t, " min")}
  
  if(any(grepl("diff", colnames(x_dat)))){
    
    if(!is.null(attr(x_dat, "states"))) {
      title <- paste0(title, " between ", attr(x_dat, "states")[1], " and ", attr(x_dat, "states")[2])
    }
    
    if(!is.null(attr(x_dat, "state_1"))) {
      title <- paste0(title, " between ", attr(x_dat, "state_1"), " and ", attr(x_dat, "state_2"))
    }
    
  }
  
  cov_heat_plot <- ggplot() + 
  geom_rect(data = x_dat, 
            mapping = aes_string(xmin = "Start", xmax = "End + 1", 
                                 ymin = "ID", ymax = "ID - 1",
                                 fill = value), 
            alpha = 0.8,
            color = "grey") +
    theme(legend.position = "bottom",
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank()) +
    labs(title = title,
         x = "Position",
         y = "",
         fill = value_label,
         color = 'Exposure') 
  
  if(grepl("diff", value)){
    
    min_du <- min(x_dat[[value]])
    max_du <- max(x_dat[[value]])
    
    cov_heat_plot <- cov_heat_plot +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", guide = guide_legend(keywidth = 3), limits = c(min_du, max_du))
  }
  
  if(grepl("err", value)){
    
    cov_heat_plot <- cov_heat_plot +
      scale_fill_gradient2(low = "white", high = "red", guide = guide_legend(keywidth = 3))
    
  }
  
  if(value == "auc"){
    
    cov_heat_plot <- cov_heat_plot +
      scale_fill_gradient2(low = "white", high = "blue", guide = guide_legend(keywidth = 3), limits = c(NA, 1))
    
  }
  
  if(value == "back_exchange"){
    
    cov_heat_plot <- cov_heat_plot +
      scale_fill_gradient2(low = "white", high = "darkorange4", limits = c(0, 100)) 
    
  }
  
  return(HaDeXify(cov_heat_plot))
}
