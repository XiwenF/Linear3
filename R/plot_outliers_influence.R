#'Plot Influential Outliers
#'
#'This set of plotting functions is used to plot
#'DFFITS, Cook's Distance, and COVRATIO separately
#'
#'@usage
#'\itemize{
#'   \item{plotdffit(lr.model)} - {Plot a DFFITS graph and indicate the subject of the outlier}
#'   \item{plotcd(lr.model)}- {Plot a Cook's Distance graph and indicate the subject of the outlier}
#'   \item{plotCVR(lr.model)} - {Plot a COVRATIO graph }
#'}
#'
#'@param lr.model Take an R object, returned by \link[Linear3]{lr}
#'
#'@examples
#'require(ggplot2)
#'##Annette Dobson (1990) "An Introduction to Generalized Linear Models".
#'##Page 9: Plant Weight Data.
#'ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#'trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#'model <- lr(ctl~trt, mtcars)
#'plotdffits(model)
#'plotcd(model)
#'plotCVR(model)
#'
#'@export
#'
#plots DFFITS and labels the subjects that are outliers
plotdffits <- function(lr.model){
  p <- lr.model$rank
  degree <- lr.model$rank + lr.model$df
  out <- 2*sqrt(p/degree)
  x <- 1:degree
  y <- dffits(lr.model)
  plotdff <- data.frame(x, y)
  colnames(plotdff) <- c("Observation", "DFFITS")
  sub <- subset(plotdff, DFFITS>out | DFFITS<(-out))
  ggplot(plotdff, aes(Observation, DFFITS, ymax = DFFITS , ymin = 0)) +
    geom_linerange(color = "red") +
    geom_hline(yintercept = out, color = "blue") +
    geom_hline(yintercept = -out, color = "blue") +
    labs(title = "Influence Diagnostics: DFFITS") +
    geom_text(data = sub ,aes(label=Observation))
}

#plots Cook's Distance and labels the subjects that are outliers
plotcd <- function(lr.model) {
  p <- lr.model$rank
  degree <- lr.model$p + lr.model$df
  x <- 1:degree
  cd <- cooks.distance(lr.model)
  plotckd <- data.frame(x, cd)
  colnames(plotckd) <- c("Observation", "CooksDist")
  subcd <- subset(plotckd, CooksDist > 4/degree )
  ggplot(plot_ckd, aes(Observation, CooksDist, ymax = CooksDist, ymin = 0)) +
    geom_linerange(color = "red") +
    geom_hline(yintercept = 4/degree , color = "blue") +
    labs(title = "Influence Diagnostics: Cook's Distance") +
    geom_text(data = subcd ,aes(label=Observation))
}

#plots COVRATIO
plotCVR <- function(lr.model){
  p <- lr.model$rank
  degree <- lr.model$p + lr.model$df
  x <- 1:degree
  cvr <- covratio(lr.model)
  out_up <- 1+3*p/degree
  out_low <- 1-3*p/degree
  plotcvr <- data.frame(x, cvr)
  colnames(plotcvr) <- c("Observation", "COVRATIO")
  subcvr <- subset(plotcvr, abs(COVRATIO-1)>3*rank/degree)
  ggplot(plotcvr, aes(Observation, COVRATIO, ymax = max(COVRATIO), ymin = min(COVRATIO))) +
    geom_point(color = "red") +
    geom_hline(yintercept = out_up , color = "blue") +
    geom_hline(yintercept = out_low , color = "blue") +
    labs(title = "Influence Diagnostics: COVRATIO")
}
