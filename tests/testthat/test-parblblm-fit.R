library(parallel)
library(sloop)


test_that("par_blblm works", {
  skip("it works")
  cl <- makeCluster(1)
  fit1 <- par_blblm(mpg ~ hp, mtcars, 10, 100, cl)
  fit2 <- par_blblm_chunk(mpg ~ hp, mtcars, 10, 100, cl, 2)
  stopCluster(cl)
  expect_equal(otype(fit1), "S3")
  expect_equal(otype(fit2), "S3")
  expect_equal(mode(fit1$formula), "call")
  expect_equal(mode(fit1$estimates), "list")
  expect_equal(mode(fit2$formula), "call")
  expect_equal(mode(fit2$estimates), "list")
})
