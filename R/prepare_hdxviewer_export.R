#' @examples 
#' kin_dat <- create_uptake_dataset(alpha_dat, states = "Alpha_KSCN" )
#' aggregated_dat <- create_aggregated_uptake_dataset(kin_dat)
#' prepare_hdxviewer_export(aggregated_dat, differential = F)
#' prepare_hdxviewer_export(aggregated_dat, differential = F, download = T)
#' 
#' 
#' 
#' @export


prepare_hdxviewer_export <- function(x_dat, 
                                     differential = FALSE,
                                     fractional = TRUE,
                                     theoretical = FALSE, 
                                     download = FALSE){

  x_dat <- aggregated_dat
  x_dat <- as.data.table(x_dat)
  
  if(differential){
    
  } else {
    
    ## fractional
    x_dat <- x_dat[, .(position, frac_deut_uptake, Exposure)]
    x_dat[, Exposure:=paste0(Exposure, "min")]  
    x_dat[, frac_deut_uptake:=frac_deut_uptake/100]
    x_dat[, .(Residues=position)]
    setnames(x_dat, "position", "Residues")
    
    res <- dcast(x_dat, Residues ~ Exposure, value.var = "frac_deut_uptake")
    
  } 
  
  if(download) {
    write.csv(res, "hdx_viewer.csv", row.names = FALSE, quote=FALSE)
    
  }
  
  return(res)
  
}


