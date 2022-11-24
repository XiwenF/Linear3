test_that("hmatrix works", {
  expect_equal(as.numeric(unlist(h_matrix(lr(mpg ~ cyl + wt, mtcars))[1])) , max(as.numeric(hatvalues(lm(mpg~cyl+wt, mtcars)))))
  expect_equal(as.numeric(unlist(h_matrix(lr(mpg ~ cyl + wt, mtcars))[2])) , as.numeric(hatvalues(lm(mpg~cyl+wt, mtcars)))[which(as.numeric(hatvalues(lm(mpg~cyl+wt, mtcars)))>2*mean(as.numeric(hatvalues(lm(mpg~cyl+wt, mtcars)))))])
})

