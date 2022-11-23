#'Plot Influential Outliers
#'
#'This set of plotting functions is used to plot DFFITS, Cook's Distance separately
#'
#'@param lm.model Take an R object, returned by \link[stats]{lm}
#'
#'@examples
#'require(ggplot2)
#'model <- lm(mpg~cyl + wt + qsec + disp, mtcars)
#'plotdffits(model)
#'plotcd(model)
#'
#'@export
#'
#plots DFFITS and labels the subjects that are outliers
plotdffits <- function(lm.model){
  p <- lm.model$rank
  degree <- lm.model$rank + lm.model$df.residual
  out <- 2*sqrt(p/degree)
  x <- 1:degree
  y <-dffits(lm.model)
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
plotcd <- function(lm.model) {
  p <- lm.model$rank
  degree <- lm.model$rank + lm.model$df.residual
  x <- 1:degree
  cd <- cooks.distance(lm.model)
  plotckd <- data.frame(x, cd)
  colnames(plotckd) <- c("Observation", "CooksDist")
  subcd <- subset(plotckd, CooksDist > 4/degree )
  ggplot(plotckd, aes(Observation, CooksDist, ymax = CooksDist, ymin = 0)) +
    geom_linerange(color = "red") +
    geom_hline(yintercept = 4/degree , color = "blue") +
    labs(title = "Influence Diagnostics: Cook's Distance") +
    geom_text(data = subcd ,aes(label=Observation))
}

