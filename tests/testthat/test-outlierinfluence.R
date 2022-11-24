test_that("outlierinfluence works", {
  expect_equal(outlierinfluence(mtcars, lr(mpg ~ cyl + wt, mtcars), option = "dffits") , as.numeric(dffits(lm(mpg~cyl+wt, mtcars))))
  expect_equal(outlierinfluence(mtcars, lr(mpg ~ cyl + wt, mtcars), option = "cd") , as.numeric(cooks.distance(lm(mpg~cyl+wt, mtcars))))
  expect_equal(as.numeric(unlist(outlierinfluence(mtcars, lr(mpg ~ cyl + wt, mtcars), option = "dffits",high.influence = TRUE))) , c(as.numeric(dffits(lm(mpg~cyl+wt, mtcars))),as.numeric(dffits(lm(mpg~cyl+wt, mtcars)))[which(abs(as.numeric(dffits(lm(mpg~cyl+wt, mtcars))))>2*sqrt(lm(mpg~cyl+wt, mtcars)$rank/nrow(mtcars)))]))
  expect_equal(as.numeric(unlist(outlierinfluence(mtcars, lr(mpg ~ cyl + wt, mtcars), option = "cd",high.influence = TRUE))) , c(as.numeric(cooks.distance(lm(mpg~cyl+wt, mtcars))),as.numeric(cooks.distance(lm(mpg~cyl+wt, mtcars)))[which(abs(as.numeric(cooks.distance(lm(mpg~cyl+wt, mtcars))))>4/nrow(mtcars))]))
  })
