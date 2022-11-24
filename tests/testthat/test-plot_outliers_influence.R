test_that("outlierinfluence works", {
  expect_equal(plotoutliers(lm(mpg~cyl+wt, mtcars),option= "dffits"),plotoutliers(lm(mpg~cyl+wt, mtcars),option= "dffits"))
  expect_equal(plotoutliers(lm(mpg~cyl+wt, mtcars),option= "cd"),plotoutliers(lm(mpg~cyl+wt, mtcars),option= "cd"))
  })
