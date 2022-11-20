outlier_influence <- function(data, lr.model, option = c("dffits", "cd" , "cvr"), high.influence = FALSE){
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
