anova <- function(formula, data, type, na.action = 'omit'){
  #get indexes of which covariates to keep
  covar<-all.vars(formula)
  index<-rep(0,length(covar))
  for (i in 1:length(covar)){
    index[i]<-which(colnames(data)==covar[i])
  }
  data<-data[,index]
  n <- nrow(data)
  if(include.intercept == TRUE){
    p <- length(labels(terms(formula))) + 1
    X <- matrix(c(rep(1,n), as.matrix(Cdata[labels(terms(formula))])), n, p)
  } else {
    p <- length(labels(terms(formula)))
    X <- as.matrix(data[labels(terms(formula))], n, p)
  }
  Y <- as.matrix(data[as.character(formula[[2]])], n, 1)

  # Dimensional inspection
  if(nrow(Y) != nrow(X)) {
    stop("The number of predicted value and observed values does not match.")
  } else if(nrow(X) < ncol(X)) {
    stop("The number of observed values is less than the predicted value.")
  }

  # Betas
  betas <- solve(t(X) %*% X) %*% t(X) %*% Y
  if(p == (length(labels(terms(formula))) + 1)){
    rownames(betas) <- c("intercept", labels(terms(formula)))
    colnames(betas) <- "Coefficients"
  } else {
    rownames(betas) <- labels(terms(formula))
    colnames(betas) <- "Coefficients"
  }

  #Fitted values
  fitted <- X %*% betas
  names(fitted) <- row.names(data)

  resid <- Y - fitted()
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
    rownames(summary) <- c("(Intercept)",covariates, "Residuals")
  }
  return(summary)
}
