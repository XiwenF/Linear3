test_that("ANOVA works", {
  expect_equal(as.numeric(unlist(ANOVA(mpg ~ cyl + wt, mtcars, type = "Sequential")["Sum Sq"])) , anova(lm(mpg ~ cyl + wt, mtcars))$"Sum Sq")
  expect_equal(as.numeric(unlist(ANOVA(mpg ~ cyl + wt, mtcars, type = "Sequential")["Df"])) , anova(lm(mpg ~ cyl + wt, mtcars))$"Df")
  expect_equal(as.numeric(unlist(ANOVA(mpg ~ cyl + wt, mtcars, type = "Sequential")["Mean Sq"])) , anova(lm(mpg ~ cyl + wt, mtcars))$"Mean Sq")
  expect_equal(as.numeric(unlist(ANOVA(mpg ~ cyl + wt, mtcars, type = "Sequential")["F value"])) , anova(lm(mpg ~ cyl + wt, mtcars))$"F value")
  expect_equal(as.numeric(unlist(ANOVA(mpg ~ cyl + wt, mtcars, type = "Sequential")["Pr(>F)"])) , anova(lm(mpg ~ cyl + wt, mtcars))$"Pr(>F)")
  expect_equal(as.numeric(ANOVA(mpg ~ wt + cyl, mtcars, type = "Partial")["Sum Sq"][3,]), anova(lm(mpg ~ wt + cyl, mtcars))$"Sum Sq"[2])
  expect_equal(as.numeric(ANOVA(mpg ~ wt + cyl, mtcars, type = "Partial")["Df"][3,]), anova(lm(mpg ~ wt + cyl, mtcars))$"Df"[2])
  expect_equal(as.numeric(ANOVA(mpg ~ wt + cyl, mtcars, type = "Partial")["F value"][3,]), anova(lm(mpg ~ wt + cyl, mtcars))$"F value"[2])
  expect_equal(as.numeric(ANOVA(mpg ~ wt + cyl, mtcars, type = "Partial")["Pr(>F)"][3,]), anova(lm(mpg ~ wt + cyl, mtcars))$"Pr(>F)"[2])
})
test_that("dimension check", {
  show_condition <- function(code) {
    tryCatch(code,
             error = function(c) "error",
             warning = function(c) "warning",
             message = function(c) "message"
    )
  }
  # Y is longer than X
  Y = matrix(c(2,3,4,5,6),5,1)
  X = matrix(c(1.1,2.2,3.3,4.5),4,1)
  expect_equal(show_condition(lr(Y, X)),"error")

  # q is greater than n (3>2 in the following case)
  Y = matrix(c(2,3),2,1)
  X = matrix(c(1.2,2.2,3.4,2.3,3.5,4.4),2,3)
  expect_equal(show_condition(lr(Y, X)),"error")
})
