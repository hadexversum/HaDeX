#' 
#' @importFrom ggplot2 ggplot_build
#' 
#' @export

plot_aggregated_uptake_structure <- function(aggregated_dat, 
                                             differential = F, 
                                             time_t, 
                                             pdb_file_path){
  
  
  aggregated_dat <- as.data.table(aggregated_dat)
  aggregated_dat <- aggregated_dat[Exposure == time_t]
  
  if(differential){
    
    tmp_plt <- ggplot(aggregated_dat) +
      geom_tile(aes(x = position, y = as.factor(Exposure), fill = diff_frac_uc)) +
      scale_fill_gradient2(low ="blue", mid = "white", high = "red") 
    
  } else {
    
    tmp_plt <- ggplot(aggregated_dat) +
      geom_tile(aes(x = position, y = as.factor(Exposure), fill = frac_uc)) +
      scale_fill_gradient2(low = "white", high = "red") 
    
  }
  
  
  plot_colors <- ggplot_build(tmp_plt)$data[[1]]["fill"]
  color_vector <- paste0("\"", paste0(plot_colors[[1]], collapse = "\",\""), "\"")
  
  r3dmol::r3dmol(
    viewer_spec = r3dmol::m_viewer_spec(
      cartoonQuality = 10,
      lowerZoomLimit = 50,
      upperZoomLimit = 350
    ),
    id = "blank_structure",
    elementId = "blank_structure",
    backgroundColor = "#FFFFFF") %>%
    r3dmol::m_add_model(data = pdb_file_path,
                        format = "pdb") %>%
    r3dmol::m_set_style(
      style = r3dmol::m_style_cartoon(color = "white")) %>%
    r3dmol::m_zoom_to() %>%
    r3dmol::m_set_style(
      style = r3dmol::m_style_cartoon(
        colorfunc = paste0("
        function(atom) {
          const color = [", color_vector, "];
          return color[atom.resi];
        }")
      ))
}

