#' Create p-value dataset
#' 
#' @importFrom data.table setcolorder
#' 
#' @param dat data imported by the \code{\link{read_hdx}} function.
#' @param protein chosen protein. 
#' @param state_1 biological state for chosen protein. From this state values
#' the second state values are subtracted to get the deuterium uptake difference.
#' @param state_2 biological state for chosen protein. This state values are 
#' subtracted from the first state values to get the deuterium uptake difference.
#' @param p_adjustment_method method of adjustment P-values for multiple 
#' comparisons. Possible methods: "BH" (Benjamini & Hochberg correction), 
#' "bonferroni" (Bonferroni correction) and "none" (default).
#' @param confidence_level confidence level for the t-test.
#' 
#' @details ...
#' 
#' @return a \code{\link{data.frame}} object.
#' 
#' @seealso 
#' \code{\link{read_hdx}}
#' \code{\link{calculate_exp_masses_per_replicate}}
#' \code{\link{plot_volcano}}
#' \code{\link{create_diff_uptake_dataset}}
#' \code{\link{create_p_diff_uptake_dataset}}
#' 
#' @examples 
#' dat <- read_hdx(system.file(package = "HaDeX", "HaDeX/data/KD_180110_CD160_HVEM.csv"))
#' p_dat <- calculate_p_value(dat)
#' head(p_dat)
#' 
#' @export calculate_p_value

calculate_p_value <- function(dat, 
                              protein = unique(dat[["Protein"]])[1],
                              state_1 = unique(dat[["State"]])[1],
                              state_2 = unique(dat[["State"]])[2],
                              p_adjustment_method = "none",
                              confidence_level = 0.98){
  
  p_adjustment_method <- match.arg(p_adjustment_method, c("none", "BH", "bonferroni"))
  
  dat <- dat[dat[["Protein"]] == protein, ]
  
  proton_mass <- 1.00727647
  
  
  tmp_dat <- data.table(calculate_exp_masses_per_replicate(dat))
  
  tmp_dat <- tmp_dat[,  .(avg_mass = mean(avg_exp_mass),
                          err_avg_mass = sd(avg_exp_mass)/sqrt(uniqueN(.SD)),
                          masses = list(avg_exp_mass)),
                     by = c("Sequence", "Start", "End", "State", "Exposure", 
                            "Modification")]
  setorderv(tmp_dat, cols = c("Start", "End"))
  
  
  tmp_dat_1 <- tmp_dat[State == state_1]
  tmp_dat_1 <- setnames(tmp_dat_1,
                        c("avg_mass", "err_avg_mass", "masses"),
                        c("avg_mass_1", "err_avg_mass_1", "masses_1"))
  tmp_dat_1[["State"]] <- NULL
  
  
  tmp_dat_2 <- tmp_dat[State == state_2]
  tmp_dat_2 <- setnames(tmp_dat_2,
                        c("avg_mass", "err_avg_mass", "masses"),
                        c("avg_mass_2", "err_avg_mass_2", "masses_2"))
  tmp_dat_2[["State"]] <- NULL
  
  
  vol_dat <- merge(tmp_dat_1, tmp_dat_2, by = c("Sequence", "Start", "End", "Exposure", "Modification"))
  
  p_dat <- rbindlist(lapply(1:nrow(vol_dat), function(i){
    
    st_1 <- vol_dat[i, "masses_1"][[1]][[1]]
    st_2 <- vol_dat[i, "masses_2"][[1]][[1]]
    
    if (length(st_1) == 1 || 
        length(st_2) == 1 || 
        (length(st_1) == length(st_2) && all(st_1 == st_2))) {
      p_value <- NA
    } else {
      p_value <- t.test(x = st_1, y = st_2, paired = FALSE, alternative = "two.sided", conf.level = confidence_level)$p.value
    }
    
    data.table(Protein = protein,
               Sequence = vol_dat[i, "Sequence"][[1]],
               Exposure = vol_dat[i, "Exposure"][[1]],
               Modification = vol_dat[i, "Modification"][[1]],
               Start = vol_dat[i, "Start"][[1]],
               End = vol_dat[i, "End"][[1]],
               P_value = p_value)
    
    
  }))
  
  p_dat[["P_value"]] <- p.adjust(p_dat[["P_value"]], method = p_adjustment_method)
  p_dat[["log_p_value"]] <- -log(p_dat[["P_value"]])
  
  setorderv(p_dat, cols = c("Protein", "Start", "End", "Exposure"))

  attr(p_dat, "protein") <- protein
  attr(p_dat, "state_1") <- state_1
  attr(p_dat, "state_2") <- state_2
  attr(p_dat, "confidence_level") <- confidence_level
  attr(p_dat, "p_adjustment_method") <- p_adjustment_method
  attr(p_dat, "has_modification") <- attr(dat, "has_modification")
  
  return(p_dat)
  
}



