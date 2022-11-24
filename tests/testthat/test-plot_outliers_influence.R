test_that("outlierinfluence works", {
  model <- lm(mpg~cyl + wt + qsec + disp, mtcars)
  expect_equal(plotoutliers(model,option = "dffits"),plotoutliers(model,option= "dffits"))
  expect_equal(plotoutliers(model,option = "cd"),plotoutliers(model,option= "cd"))
  })
