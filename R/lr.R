# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

lr <- function(formula,data, include.intercept = TRUE, to.predict = NULL, na.action = 'omit') {
  #get indexes of which covariates to keep
  covariates<-all.vars(formula)
  index<-rep(0,length(covariates))
  for (i in 1:length(covariates)){
    index[i]<-which(colnames(data)==covariates[i])
  }
  data<-data[,index]

  #Deal with missing value
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

  # dimension check
 if(nrow(Y) != nrow(X)) {
   stop("Number of the outcomes and observations do not match.")
 } else if(nrow(X) < ncol(X)) {
   stop("Number of observations is less than predictors.")
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
 std.error <- sqrt(diag(var_cov_beta))
 t_value <- betas / std.error
 p_value <- rep(0, length(betas))
 for (i in 1:length(betas)) {
   p_value[i] <- 2*pt(q=abs(t_value[i]), df=df, lower.tail=FALSE)
 }
 Coeff_summary <- cbind(betas, std.error, t_value, p_value)
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
 LB <- betas - qt(0.05/2, df, lower.tail = FALSE)*std.error
 UB <- betas + qt(0.05/2, df, lower.tail = FALSE)*std.error
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
 if(!is.null(to.predict)){
   if(include.intercept == TRUE){
     predicted <- cbind(1, to.predict) %*% beta
     colnames(predicted) <- "Predicted Values"
   } else {
     predicted <- to.predict %*% beta
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



