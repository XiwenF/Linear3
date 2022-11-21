#'outlierinfluence
#'
#'outlierinfluence computes three diagnostic statistics for measuring points of influence:
#' \describe{
#'   \item{DIFFTS}{Difference in the fitted value when ith observation is removed}
#'   \item{Cook's Distance}{shows the influence of each observation on the fitted response values}
#'   \item{COVRATIO}{influence on variance-covariance matrix}
#' }
#'
#'@usage
#'outlierinfluence(data, lr.model,
#'   option = c("dffits", "cd" , "cvr"),
#'     high.influence = FALSE)
#'
#'@param data A data frame containing the variables in the model
#'@param lr.model Take an R object, returned by \link[Linear3]{lr}
#'@param option Which measure affecting the diagnosis should be calculated.
#'"dffits" refers to DFFITS, "cd" refers to Cook's distance,
#'"cvr" refers to COVRATIO
#'@param high.influence (Default)FALSE; if TRUE, influential observations and their
#'respective diagnostic statistics will be returned.
#'
#'
#'
#'@return  If high.influence is set to "TRUE", each diagnostic stat will return
#'a list containing two elements.
#'\itemize{
#'   \item{dffits, Cook's Distance, COVRATIO} - {depends on which option is chosen}
#'   \item{outliers} - {which observations are considered influential}
#' }
#'
#'@examples
#'data(mtcars)
#'attach(mtcars)
#'model <- lr(mpg~cyl + wt, mtcars)
#'outlierinfluence(mtcars, model, option = c("dffits"))
#'outlierinfluence(mtcars, model, option = c("cd"))
#'outlierinfluence(mtcars, model, option = c("cvr"), high.influence = TRUE)
#'
#'@export
#'
outlierinfluence <- function(data, lr.model, option = c("dffits", "cd" , "cvr"), high.influence = FALSE){
  ex_res <- res_3(data,lr.model, r="ex")
  int_res <- res_3(data, lr.model, r="int")
  fit_res <- lr.model$residuals
  n <- nrow(data)
  p <- formula$rank
  MSE <- sum(fit_res^2)/(n-p)
  X <- model.matrix(lr.model)
  lev <- hatvalues(lr.model)
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
  #influence on variance-covariance matrix
  } else if (option == "cvr") {
    matrix.data <- as.matrix(data)
    name_y <- as.character(lr.model$term[[2]])
    y <- matrix.data[,name_y]
    df_res <- n-p-1
    ex_model <- .Call(stats:::C_Cdqrls, X, y, 1e-7, FALSE)
    MSE_i <- (fit_res/(ex_res * sqrt(1-lev)))^2
    cvr <- (MSE_i/MSE)^p*(1/(1-lev))
    out_cvr <- cvr[which((abs(cvr-1)>3*p/n))]
    result_cvr <- list(cvr, out_cvr)
    names(result_cvr) <- c("COVRATIO", "outliers")
    if (high.influence){
      return(result_cvr)
    } else {
      return(cvr)
    }
  }
}
