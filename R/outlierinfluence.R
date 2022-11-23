#'outlierinfluence
#'
#'outlierinfluence computes two diagnostic statistics for measuring points of influence:
#' \describe{
#'   \item{DIFFTS}{Difference in the fitted value when ith observation is removed}
#'   \item{Cook's Distance}{shows the influence of each observation on the fitted response values}
#' }
#'
#'@usage
#'outlierinfluence(data, lr.model,
#'   option = c("dffits", "cd"),
#'     high.influence = FALSE)
#'
#'@param data A data frame containing the variables in the model
#'@param lr.model Take an R object, returned by \link[Linear3]{lr}
#'@param option Which measure affecting the diagnosis should be calculated.
#'"dffits" refers to DFFITS, "cd" refers to Cook's distance
#'@param high.influence (Default)FALSE; if TRUE, influential observations and their
#'respective diagnostic statistics will be returned.
#'
#'
#'
#'@return  If high.influence is set to "TRUE", each diagnostic stat will return
#'a list containing two elements.
#'\itemize{
#'   \item{dffits, Cook's Distance} - {depends on which option is chosen}
#'   \item{outliers} - {which observations are considered influential}
#' }
#'
#'@examples
#'data(mtcars)
#'attach(mtcars)
#'model <- lr(mpg~cyl + wt + qsec + disp, mtcars)
#'outlierinfluence(mtcars, model, option = c("dffits"))
#'outlierinfluence(mtcars, model, option = c("cd"))
#'
#'@export
#'
outlierinfluence <- function(data, lr.model, option = c("dffits", "cd"), high.influence = FALSE){
  ex_res <- lr.model$ex_stud_res
  int_res <- lr.model$studentized_res
  fit_res <- lr.model$residuals
  n <- nrow(data)
  p<-lr.model$rank
  MSE <- sum(fit_res^2)/(n-p)
  lev <- lr.model$leverage
  # Difference in the fitted value when ith observation is removed
  if (option == "dffits"){
    dff <- ex_res*sqrt(lev/(1-lev))
    out_dff <- dff[which(abs(dff)>2*sqrt(p/n))]
    result_dff <- list(dff, out_dff)
    names(result_dff) <- c("dffits", "outliers")
    if (high.influence){
      return(result_dff)
    } else {
      return(dff)
    }
  #shows the influence of each observation on the fitted response values
  } else if ( option == "cd") {
    cd <- int_res^2/p * lev/(1-lev)
    out_cd <- cd[which(cd>4/n)]
    result_cd <- list(cd, out_cd)
    names(result_cd) <- c("Cook's Distance", "outliers")
    if (high.influence){
      return(result_cd)
    } else {
      return(cd)
    }
 }
  }

