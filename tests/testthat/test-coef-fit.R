test_that("coef works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  cf <- coef(fit)
  parm <- attr(terms(fit$formula), "term.labels")
  expect_equal(length(cf), length(parm) + 1)
})
