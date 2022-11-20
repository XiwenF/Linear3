#plots DFFITS and labels the subjects that are outliers
plot_dffits <- function(lr.model){
  p <- lr.model$rank
  degree <- lr.model$rank + lr.model$df
  out <- 2*sqrt(p/degree)
  x <- 1:degree
  y <- dffits(lr.model)
  plot_dff <- data.frame(x, y)
  colnames(plot_dff) <- c("Observation", "DFFITS")
  sub <- subset(plot_dff, DFFITS>out | DFFITS<(-out))
  ggplot(plot_dff, aes(Observation, DFFITS, ymax = DFFITS , ymin = 0)) +
    geom_linerange(color = "red") +
    geom_hline(yintercept = out, color = "blue") +
    geom_hline(yintercept = -out, color = "blue") +
    labs(title = "Influence Diagnostics: DFFITS") +
    geom_text(data = sub ,aes(label=Observation))
}

#plots Cook's Distance and labels the subjects that are outliers
plot_cd <- function(lr.model) {
  p <- lr.model$rank
  degree <- lr.model$p + lr.model$df
  x <- 1:degree
  cd <- cooks.distance(lr.model)
  plot_ckd <- data.frame(x, cd)
  colnames(plot_ckd) <- c("Observation", "CooksDist")
  sub_cd <- subset(plot_ckd, CooksDist > 4/degree )
  ggplot(plot_ckd, aes(Observation, CooksDist, ymax = CooksDist, ymin = 0)) +
    geom_linerange(color = "red") +
    geom_hline(yintercept = 4/degree , color = "blue") +
    labs(title = "Influence Diagnostics: Cook's Distance") +
    geom_text(data = sub_cd ,aes(label=Observation))
}

#plots COVRATIO
plot_CVR <- function(lr.model){
  p <- lr.model$rank
  degree <- lr.model$p + lr.model$df
  x <- 1:degree
  cvr <- covratio(lr.model)
  out_up <- 1+3*p/degree
  out_low <- 1-3*p/degree
  plot_cvr <- data.frame(x, cvr)
  colnames(plot_cvr) <- c("Observation", "COVRATIO")
  sub_cvr <- subset(plot_cvr, abs(COVRATIO-1)>3*rank/degree)
  ggplot(plot_cvr, aes(Observation, COVRATIO, ymax = max(COVRATIO), ymin = min(COVRATIO))) +
    geom_point(color = "red") +
    geom_hline(yintercept = out_up , color = "blue") +
    geom_hline(yintercept = out_low , color = "blue") +
    labs(title = "Influence Diagnostics: COVRATIO")
}
