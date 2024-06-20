#' @importFrom purrr map_chr
#'
#'
#' @export

get_residue_positions <- function(dat){
  
  clean_dat <- dat[, Sequence := map_chr(strsplit(Sequence, "[+]"), 1)]
  
  x <- do.call(rbind, lapply(1:nrow(clean_dat), function(i){
    data.frame(position = clean_dat[i, Start]:clean_dat[i, End],
               aa = strsplit(clean_dat[i, Sequence], split="")[[1]])
  })) %>% unique(.) %>% arrange(position)
  
  x_blank <- data.frame(position = 1:max(x[["position"]]))
  
  merge(x, x_blank, by = "position", all.y = T)
  
}