% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlierinfluence.R
\name{outlierinfluence}
\alias{outlierinfluence}
\title{outlierinfluence}
\usage{
outlierinfluence(data, lr.model,
  option = c("dffits", "cd"),
    high.influence = FALSE)
}
\arguments{
\item{data}{A data frame containing the variables in the model}

\item{lr.model}{Take an R object, returned by \link[Linear3]{lr}}

\item{option}{Which measure affecting the diagnosis should be calculated.
"dffits" refers to DFFITS, "cd" refers to Cook's distance}

\item{high.influence}{(Default)FALSE; if TRUE, influential observations and their
respective diagnostic statistics will be returned.}
}
\value{
If high.influence is set to "TRUE", each diagnostic stat will return
a list containing two elements.
\itemize{
  \item{dffits, Cook's Distance} - {depends on which option is chosen}
  \item{outliers} - {which observations are considered influential}
}
}
\description{
outlierinfluence computes two diagnostic statistics for measuring points of influence:
\describe{
  \item{DIFFTS}{Difference in the fitted value when ith observation is removed}
  \item{Cook's Distance}{shows the influence of each observation on the fitted response values}
}
}
\examples{
data(mtcars)
attach(mtcars)
model <- lr(mpg~cyl + wt + qsec + disp, mtcars)
outlierinfluence(mtcars, model, option = c("dffits"))
outlierinfluence(mtcars, model, option = c("cd"))

}
