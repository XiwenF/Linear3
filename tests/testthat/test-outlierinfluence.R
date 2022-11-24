test_that("outlierinfluence works", {
  expect_equal(outlierinfluence(mtcars, lr(mpg ~ cyl + wt, mtcars), option = "dffits") , as.numeric(dffits(lm(mpg~cyl+wt, mtcars))))
  expect_equal(outlierinfluence(mtcars, lr(mpg ~ cyl + wt, mtcars), option = "cd") , as.numeric(cooks.distance(lm(mpg~cyl+wt, mtcars))))
})
