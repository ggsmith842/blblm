test_that("prediction works", {
  fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  pfit<-predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
  pfitCI<-predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)), confidence = TRUE)
  expect_equal(length(pfit), nrow(pfitCI))
  expect_equivalent(pfit,pfitCI[,1])

})
