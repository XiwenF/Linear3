#'lr
#'
#'A fitted linear regression model was used
#'
#'@param formula An object of class "formula" which is the model to fit the regression
#'
#'@param data Data frame to perform the linear regression on
#'
#'@param include.intercept If the model should be fitted with an intercept, include.intercept = TRUE; if model should be fitted without an intercept,
#'then include.intercept = FALSE. The default setting for include.intercept is TRUE.
#'
#'@param predict The default parameter is set to NULL. If you want to use the current model for prediction, enter an n by p matrix (or
#'object coercible by as.matrix to a matrix) to get predicted values. n is the number of prediction desired and p is the number of covariates included in the model.
#'
#'@param na.action Character input, which determines how the regression model should handle missing data. Options include 'omit' (remove rows with missing values), 'fail'(stop regression), or 'impute' (replace missing values with column mean). Defaults to omit if excluded from call.
#'
#'@return lr doesn't explicitly return anything unless you extract the value with $ followed by the name of the desired output.
#'The returned output is a list containing at least the following:
#'\describe{
#'  \item{coefficients}{a named coefficients vector}
#'  \item{residuals}{Residuals (i.e. response values minus fitted values)}
#'  \item{standardized_res}{the standardized residuals}
#'  \item{studentized_res}{the internally studentized residuals}
#'  \item{ex_stud_res}{the externally studentized residuals}
#'  \item{fitted.values}{the predicted value}
#'  \item{sigma}{the residual standard error}
#'  \item{hat_matrix}{the matrix maps the vector of response values (dependent variable values) to the vector of fitted values (or predicted values)}
#'  \item{leverage}{obtain leverage}
#'  \item{df}{degrees of freedom}
#'  \item{rank}{the number of betas}
#'  \item{coeff_summary}{mimic the results using summary(lm()) including estimates the beta coefficients, standard errors, t values, and p-value}
#'  \item{R_squared}{the proportion of the variation in the dependent variable explained by the independent variable(s)}
#'  \item{adj_R_squared}{a penalized version of R_squared}
#'  \item{CI}{95\% confidence interval of estimates (i.e. coefficients)}
#'  \item{Fstatistic}{give the overall F statistic and its corresponding degrees of freedom of numerator and denominator}
#'  \item{p_value_F_test}{p-value for overall F test}
#'  \item{predicted}{give the predicted value using the current fitted model}
#' }
#'
#'@examples
#'data(mtcars)
#'attach(mtcars)
#'lr(mpg ~ cyl + wt , mtcars)$coefficients ## Obtain beta coefficient estimates
#'lr(mpg ~ cyl + wt, mtcars)$coeff_summary ## Obtain summary of beta coefficients
#'lr(mpg ~ cyl + wt, mtcars)$sigma ## Obtain residual standard error
#'lr(mpg ~ cyl + wt, mtcars)$CI ## Obtain 95% confidence interval
#'lr(mpg ~ cyl + wt, mtcars, include.intercept = FALSE) ## omitting intercept
#'lr(mpg ~ cyl + wt, mtcars, include.intercept = FALSE)$df ## Extract degrees of freedom when fitting a model without an intercept
#'lr(mpg ~ cyl + wt, mtcars, predict = matrix(c(mean(mtcars$mpg), mean(mtcars$wt)), 1, 2))$predicted
#'detach(mtcars)
#'
#'@importFrom stats model.matrix na.omit pf pt quantile
#'
#'@export
#'
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
   X <- matrix(c(rep(1,n), as.matrix(data[labels(terms(formula))])), n, p)
 } else {
   p <- length(labels(terms(formula)))
   X <- as.matrix(data[labels(terms(formula))], n, p)
 }
 Y <- as.matrix(data[as.character(formula[[2]])], n, 1)


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

 # Residuals
 resid <- as.vector(Y - fitted)
 sigma <- sqrt((t(resid) %*% resid) / (n-p))
 ## Standardized residuals
 zi <- resid/as.numeric(sigma)
 Hat <- X %*% solve(t(X) %*% X) %*% t(X)
 leverage <- diag(Hat)
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
     predicted <- cbind(1, predict) %*% betas
     colnames(predicted) <- "Predicted Values"
   } else {
     predicted <- predict %*% betas
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
                fitted.values = fitted,
                sigma = as.vector(sigma),
                hat_matrix = Hat,
                leverage = leverage,
                df = df,
                rank = p,
                coeff_summary = Coeff_summary,
                R_squared = as.vector(R2),
                adj_R_squared = as.vector(R2_adj),
                CI = CI_95,
                Fstatistic = F_stat_df,
                p_value_F_test = as.vector(p_val_F),
                predicted = predicted)
 return(invisible(results))
}

