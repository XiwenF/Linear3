test_that("lr works", {
  expect_equal(as.numeric(lr(mpg ~ cyl + wt, mtcars)$coefficients), as.numeric(lm(mpg ~ cyl + wt, mtcars)$coefficients))
  expect_equal(as.numeric(lr(mpg ~ cyl + wt, mtcars)$residuals), as.numeric(summary(lm(mpg ~ cyl + wt, mtcars))$residuals))
  expect_equal(as.numeric(lr(mpg ~ cyl + wt + disp, mtcars)$fitted.values), as.numeric(lm(mpg ~ cyl + wt + disp, mtcars)$fitted.values))
  expect_equal(lr(mpg ~ cyl + wt, mtcars)$sigma, summary(lm(mpg ~ cyl + wt, mtcars))$sigma)
  expect_equal(as.numeric(lr(mpg ~ cyl + wt + disp, mtcars)$coeff_summary), as.numeric(summary(lm(mpg ~ cyl + wt + disp, mtcars))$coefficients))
  expect_equal(lr(mpg ~ cyl + wt, mtcars)$R_squared, summary(lm(mpg ~ cyl + wt, mtcars))$r.squared)
  expect_equal(lr(mpg ~ cyl + wt, mtcars)$adj_R_squared, summary(lm(mpg ~ cyl + wt, mtcars))$adj.r.squared)
  expect_equal(as.numeric(lr(mpg ~ cyl + wt + qsec, mtcars)$CI), as.numeric(confint(lm(mpg ~ cyl + wt + qsec, mtcars))))
  expect_equal(lr(mpg ~ cyl + wt, mtcars, include.intercept = FALSE)$sigma, summary(lm(mpg ~ -1 + cyl + wt, mtcars))$sigma)
  expect_equal(as.numeric(lr(mpg~cyl+wt, mtcars)$Fstatistic), as.numeric(summary(lm(mpg~cyl+wt, mtcars))$fstatistic))
  expect_equal(as.numeric(lr(mpg~cyl+wt, mtcars)$p_value_F_test), as.numeric(pf(summary(lm(mpg~cyl+wt, mtcars))$fstatistic[1], summary(lm(mpg~cyl+wt, mtcars))$fstatistic[2], summary(lm(mpg~cyl+wt, mtcars))$fstatistic[3], lower.tail = FALSE)))
  expect_equal(as.numeric(lr(cyl~mpg+wt, mtcars, predict = matrix(c(mean(mtcars$mpg), mean(mtcars$wt)), 1, 2))$predicted), as.numeric(predict(lm(cyl~mpg+wt,mtcars), newdata=data.frame(mpg=mean(mtcars$mpg), wt = mean(mtcars$wt)), se.fit=TRUE)$fit))
  expect_equal(as.numeric(lr(cyl~mpg+wt, mtcars, include.intercept = FALSE, predict = matrix(c(mean(mtcars$mpg), mean(mtcars$wt)), 1, 2))$predicted), as.numeric(predict(lm(cyl~-1+mpg+wt,mtcars), newdata=data.frame(mpg=mean(mtcars$mpg), wt = mean(mtcars$wt)), se.fit=TRUE)$fit))
  expect_equal(as.numeric(lr(cyl~mpg+wt, mtcars)$standardized_res), as.numeric(lm(cyl~mpg+wt,mtcars)$residuals/summary(lm(cyl~mpg+wt,mtcars))$sigma))
  expect_equal(diag(lr(mpg~cyl+wt, mtcars)$hat_matrix),as.numeric(lm.influence(lm(mpg~cyl+wt, mtcars))$hat))
  expect_equal(lr(mpg~cyl+wt, mtcars)$leverage, as.numeric(hatvalues(lm(mpg~cyl+wt, mtcars))))
  expect_equal(as.numeric(lr(mpg~cyl+wt, mtcars)$studentized_res), as.numeric(rstandard(lm(mpg~cyl+wt, mtcars))))
  expect_equal(as.numeric(lr(mpg~cyl+wt, mtcars)$ex_stud_res), as.numeric(rstudent(lm(mpg~cyl+wt, mtcars))))
})

test_that("lr na.action works", {
  mtcars$cyl[1]<-NA
  expect_output(print(lr(mpg~cyl+wt, mtcars, na.action = 'omit')$coefficients))
  expect_output(print(lr(mpg~cyl+wt, mtcars, na.action = 'impute')$coefficients))
  expect_error(lr(mpg~cyl+wt, mtcars, na.action ='fail')$coefficients)
})

