lr <- function(formula,data, include.intercept = TRUE, predict = NULL, na.action = 'omit') {
  #get indices of which covariates to keep
  covar<-all.vars(formula)
  index<-rep(0,length(covar))
  for (i in 1:length(covar)){
    index[i]<-which(colnames(data)==covar[i])
  }
  data<-data[,index]

  #Handle missing values
  if(anyNA(data)==TRUE){
    if(na.action == 'omit'){
      data<-na.omit(data)
    }else if (na.action == 'fail'){
      stop("Dataset contains missing values.")
    } else if (na.action == 'impute'){
      for(i in 1:ncol(data)){
        data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
      }
    }
  }

  n<-nrow(data)
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

 # Residuals
 resid <- as.vector(y - fitted)
 names(resid) <- row.names(data)
 sigma <- sqrt((t(resid) %*% resid) / (n-p))
 ## Standardized residuals
 zi <- resid/as.numeric(sigma)
 H <- X %*% solve(t(X) %*% X) %*% t(X)
 leverage <- diag(H)
 ## studentized residuals
 ri <- resid/as.numeric(sigma)/sqrt(1-leverage)
 ## Externally studentized residuals
 r_i <- ri*sqrt((n-p-1)/(n-p-ri^2))

 #T scores and P-values
 df <- n-p
 var_cov_beta <- as.vector((t(resid) %*% resid) / (n-p)) * solve(t(X) %*% X)
 serror <- sqrt(diag(var_cov_beta))
 t_value <- betas / serror
 p_value <- rep(0, length(betas))
 for (i in 1:length(betas)) {
   p_value[i] <- 2*pt(q=abs(t_value[i]), df=df, lower.tail=FALSE)
 }
 Coeff_summary <- cbind(betas, serror, t_value, p_value)
 colnames(Coeff_summary) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
 sig <- c()
 sig <- sapply(1:length(p_value), function(i) {
   if(p_value[i] < 0.001){sig[i] <- '***'}
   else if (p_value[i] < 0.01){sig[i] <- '** '}
   else if (p_value[i] < 0.05){sig[i] <- '*  '}
   else if (p_value[i] < 0.1){sig[i] <- '.  '}
   else{sig[i] <- ''}
 })
 coef.tb <- cbind(signif(Coeff_summary,4), sig)
 colnames(coef.tb)[5] <- ""

 #R-square
 SSY<-sum((Y-mean(Y))^2)
 SSR <- sum((fitted-mean(Y))^2)
 SSE<- t(resid) %*% resid
 R2 <- SSR/SSY
 R2_adj <- 1 - (SSE/(n-p))/(SSY/(n-1))

 #95% confidence
 LB <- betas - qt(0.05/2, df, lower.tail = FALSE)*serror
 UB <- betas + qt(0.05/2, df, lower.tail = FALSE)*serror
 CI_95 <- cbind(LB, UB)
 colnames(CI_95) <- c("2.5%", "97.5%")

 #MSR, MSE and F-statistics
 numdf <- p - 1
 MSR <- SSR / numdf
 dendf <- n - p
 MSE <- SSE / dendf
 F_stat <- MSR / MSE
 F_stat_df <- c(F_stat, numdf, dendf)
 names(F_stat_df) <- c("F statistic", "numdf", "dendf")
 p_val_F <- pf(F_stat, numdf, dendf, lower.tail = FALSE)

 #predict
 if(!is.null(predict)){
   if(include.intercept == TRUE){
     predicted <- cbind(1, predict) %*% beta
     colnames(predicted) <- "Predicted Values"
   } else {
     predicted <- predict %*% beta
     colnames(predicted) <- "Predicted Values"
   }
 } else {
   predicted <- NULL
 }

 #results
 results <- list(coefficients = betas,
                residuals = resid,
                standardized_res = zi,
                studentized_res = ri,
                ex_stud_res = r_i,
                fitted.values = fitted(),
                sigma = as.vector(sigma),
                hat_matrix = H,
                leverage = leverage,
                df = df,
                rank = p,
                coeff_summary = Coeff_summary,
                R_squared = as.vector(R2),
                adj_R_squared = as.vector(R2_adj),
                CI = CI_95,
                fstatistic = F_stat_df,
                p_value_f_test = as.vector(p_val_F),
                predicted = predicted)
 return(invisible(results))
}



