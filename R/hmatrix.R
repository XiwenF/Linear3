h_matrix <- function(lr.model){
  leverage <- lr.model$leverage
  #which subject has the maximum value of the leverage
  max.lev <- leverage[which.max(leverage)]
  #what subjects are outliers in the X space, and their leverage values
  lev.gtmean <- leverage[which(leverage>2*mean(leverage))]
  result_lev <- list(max.lev, lev.gtmean)
  names(result_lev) <- c("maximum hi", "hi>2*mean(H)")
  return(result_lev)
}


