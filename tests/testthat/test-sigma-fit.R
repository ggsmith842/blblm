test_that("sigma works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  expect_equivalent(sigma(fit)[1], sigma(fit, confidence = TRUE)[1], )
})
