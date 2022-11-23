#' ANOVA
#'
#' ANOVA is used to obtain an analysis of variance table for a fitted model. It produces the same results as anova(lm( )) for sequential sums of squares
#' and car::Anova(lm( ), type="III") for partial sums of squares.
#'
#' @param formula An object of class "formula": a symbolic description of the model to be fitted. A typical model is the form outcome ~ covariates
#' where the outcome is a numeric response vector (which people usually denote as Y in statistical formulas) and the covariates are predictors of response.
#'
#' @param data A data frame (or an object convertible to a data frame by as.data.frame) containing the variables in the model.
#'
#' @param type The type of sum of squares to obtain. Use "Sequential" for sequential sums of squares and "Partial" for partial sums of squares.
#'
#' @param na.action Character input, which determines how the regression model should handle missing data. Options include 'omit' (remove rows with missing values), 'fail'(stop regression), or 'impute' (replace missing values with column mean). Defaults to omit if excluded from call.
#'
#' @return ANOVA returns anova table in a data.frame.
#'
#' @examples
#' data(mtcars)
#' attach(mtcars)
#' ANOVA(mpg ~ cyl + wt + qsec + disp, mtcars, type = "Partial") ## Get partial SS
#' ANOVA(mpg ~ cyl + wt + qsec + disp, mtcars, type = "Sequential") ## Get sequential SS
#' ANOVA(mpg ~ cyl + wt + qsec + disp, mtcars, type = "Sequential")["F value"] ## Extract F statistics from ANOVA table
#' detach(mtcars)
#'
#' @export
#'
#'
ANOVA <- function(formula, data, type, na.action = 'omit'){
  #get indices of which covariates to keep
  covar<-all.vars(formula)
  index<-rep(0,length(covar))
  for (i in 1:length(covar)){
    index[i]<-which(colnames(data)==covar[i])
  }
  data<-data[,index]
  n <- nrow(data)
  p<-length(covar)+1
  X <- matrix(c(rep(1,n), as.matrix(data[covar])), n, p)
  Y <- as.matrix(data[as.character(formula[[2]])], n, 1)

  # Dimensional inspection
  if(nrow(Y) != nrow(X)) {
    stop("The number of predicted value and observed values does not match.")
  } else if(nrow(X) < ncol(X)) {
    stop("The number of observed values is less than the predicted value.")
  }

  # Betas
  betas <- solve(t(X) %*% X) %*% t(X) %*% Y

  #Fitted values
  fitted <- X %*% betas

  resid <- Y - fitted
  SSE <- t(resid) %*% resid
  MSE <- SSE / (n - p) ## Same SSE for all SS
  if(type == "Sequential"){
    ## SSE under H0
    SSE0 <- rep(0, length(covar))
    for (i in 1:length(covar)) {
      H0 <- X[,(1:i)] %*% solve(t(X[,(1:i)]) %*% X[,(1:i)]) %*% t(X[,(1:i)])
      SSE0[i] <- as.numeric(t(Y) %*% Y - t(Y) %*% H0 %*% Y)
    }
    ## SSE under H1
    SSE1 <- rep(0, length(covar))
    for (i in 1:(p-1)) {
      H1 <- X[,(1:(i+1))] %*% solve(t(X[,(1:(i+1))]) %*% X[,(1:(i+1))]) %*% t(X[,(1:(i+1))])
      SSE1[i] <- as.numeric(t(Y) %*% Y - t(Y) %*% H1 %*% Y)
    }
    SS <- SSE0 - SSE1
    F_stat <- SS/rep(MSE,length(SS))
    p_val <- rep(0, length(F_stat))
    for (i in 1:length(F_stat)) {
      p_val[i] <- pf(F_stat[i], 1, n-p, lower.tail = FALSE)
    }
    table <- cbind(rep(1,p-1), SS, SS, F_stat, p_val)
    colnames(table) <- c("Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)")
    res <- c(n-p, SSE, MSE, " ", " ")
    summary <- as.data.frame(rbind(table, res))
    rownames(summary) <- c(covar,"Residuals")
  }
  if(type == "Partial"){
    ## SSE under H0
    SSE0 <- rep(0, p)
    for (i in 1:p) {
      H0 <- X[,-i] %*% solve(t(X[,-i]) %*% X[,-i]) %*% t(X[,-i])
      SSE0[i] <- as.numeric(t(Y) %*% Y - t(Y) %*% H0 %*% Y)
    }
    SS <- SSE0 - rep(SSE, length(SSE0))
    F_stat <- SS/rep(MSE,length(SS))
    p_val <- rep(0, length(F_stat))
    for (i in 1:length(F_stat)) {
      p_val[i] <- pf(F_stat[i], 1, n-p, lower.tail = FALSE)
    }
    table <- cbind(SS, rep(1,p), F_stat, p_val)
    colnames(table) <- c("Sum Sq", "Df", "F value", "Pr(>F)")
    res <- c(SSE, n-p, " ", " ")
    summary <- as.data.frame(rbind(table, res))
    rownames(summary) <- c("(Intercept)",covar, "Residuals")
  }
  return(summary)
}
