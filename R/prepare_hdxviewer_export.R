#' Prepares data export for HDX-Viewer
#'
#' @param x_dat one state detuerium uptake data or differentail uptake data
#' @param differential indicator of x_dat type
#' @param fractional indicator if fractional values are used
#' @param theoretical indicator if theoretical values are used
#' @param download indicator if the result should be downloaded 
#'
#' @examples
#' kin_dat <- create_uptake_dataset(alpha_dat, states = "Alpha_KSCN" )
#' aggregated_dat <- create_aggregated_uptake_dataset(kin_dat)
#' diff_uptake_dat <- create_diff_uptake_dataset(alpha_dat)
#' diff_aggregated_dat <- create_aggregated_diff_uptake_dataset(diff_uptake_dat)
#' prepare_hdxviewer_export(aggregated_dat, differential = F)
#' prepare_hdxviewer_export(aggregated_dat, differential = T) # shouldnt work
#' prepare_hdxviewer_export(diff_aggregated_dat, differential = T)
#' prepare_hdxviewer_export(aggregated_dat, differential = F, download = T)
#'
#' @export


prepare_hdxviewer_export <- function(x_dat,
                                     differential = FALSE,
                                     fractional = TRUE,
                                     theoretical = FALSE,
                                     download = FALSE){

  x_dat <- as.data.table(x_dat)

  if(differential){

    if(!any(startsWith(names(x_dat), "diff") )){
      stop("Parameters are in conflict with supplied data.")
    }

    if(fractional){

      if(theoretical){

        x_dat <- x_dat[, .(position, diff_theo_frac_deut_uptake, Exposure)]
        x_dat[, diff_theo_frac_deut_uptake:=diff_theo_frac_deut_uptake/100]
        value <- "diff_theo_frac_deut_uptake"
      } else {

        x_dat <- x_dat[, .(position, diff_frac_deut_uptake, Exposure)]
        x_dat[, diff_frac_deut_uptake:=diff_frac_deut_uptake/100]
        value <- "diff_frac_deut_uptake"
      }
    } else {

      x_dat <- x_dat[, .(position, diff_deut_uptake, Exposure)]
      value <- "diff_deut_uptake"

      if(theoretical){

        x_dat <- x_dat[, .(position, diff_theo_deut_uptake, Exposure)]
        value <- "diff_theo_deut_uptake"

      }
    }

  } else {

    if(fractional){

      if(theoretical){

        x_dat <- x_dat[, .(position, theo_frac_deut_uptake, Exposure)]
        x_dat[, theo_frac_deut_uptake:=theo_frac_deut_uptake/100]
        value <- "theo_frac_deut_uptake"

      } else {

        x_dat <- x_dat[, .(position, frac_deut_uptake, Exposure)]
        x_dat[, frac_deut_uptake:=frac_deut_uptake/100]
        value <- "frac_deut_uptake"

      }
    } else {

      x_dat <- x_dat[, .(position, deut_uptake, Exposure)]
      value <- "deut_uptake"

      if(theoretical){

        x_dat <- x_dat[, .(position, theo_deut_uptake, Exposure)]
        value <- "theo_deut_uptake"

      }

    }

  }

  x_dat[, Exposure:=paste0(Exposure, "min")]
  x_dat[, .(Residues=position)]
  setnames(x_dat, "position", "Residues")

  res <- dcast(x_dat, Residues ~ Exposure, value.var = value)



  if(download) {
    write.csv(res, paste0(value, ".csv"), row.names = FALSE, quote=FALSE)

  }

  return(res)

}


