#'h_matrix
#'
#'Finds the index of the maximum leverage value
#'
#'@param lr.model Take an R object, returned by \link[Linear3]{lr}
#'
#'@return  A checklist that includes the following:
#' \itemize{
#'   \item{maximum hi} - {Which entity has the maximum value of the leverage}
#'   \item{hi>2*mean(H)} - {Which objects are outliers in the X-space, and their leverage values}
#' }
#'
#'@examples
#'data(mtcars)
#'attach(mtcars)
#'model <- lr(mpg ~ cyl + wt, mtcars)
#'h_matrix(model)
#'h_matrix(model)[1]
#'h_matrix(model)[2]
#'detach(mtcars)
#'
#'@export
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


