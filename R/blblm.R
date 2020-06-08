#' @import purrr
#' @import stats
#' @import parallel
#' @import sloop
#' @importFrom magrittr %>%
#' @importFrom utils capture.output
#'
#' @aliases NULL
#' @details
#' Linear Regression with Little Bag of Bootstraps
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c(".", "fit"))

#' Calculate coefficeints using bag of little bootstraps
#'
#'
#'
#' @param formula the features of interest from the data
#' @param data the dataset of interest
#' @param m the number of subsamples of the data
#' @param B the number of bootstrap samples to take
#'
#' @return the fitted lm for the bag of little bootstraps
#'
#' @export
#' @examples
#' blblm(hp ~ mpg, mtcars, 10, 100)
blblm <- function(formula, data, m = 10, B = 5000) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B)
  )
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

#' Computes the blblm results using parallization with user-specified number of cores
#'
#' @param formula an lm formula
#' @param data the dataset containing the features mentioned in formula
#' @param m the number of subsamples the data is to be split into
#' @param B the number of bootstrap samples to take
#' @param cl this is equvalent to makeCluster(). This is created by the user outside the funtion.
#' @return list
#'
#' @export
#' @examples
#' \dontrun{
#' cl <- makeCluster(4)
#' par_blblm(mpg ~ hp, mtcars, 10, 100, cl)
#' stopCluster(cl)
#' }
par_blblm <- function(formula, data, m = 10, B = 5000, cl) {
  data_list <- split_data(data, m)
  estimates <- parLapply(cl, data_list, function(formula, data, n, B) { #use parlapply
    lm_each_subsample(formula = formula, data = data, n = nrow(data), B = B)
  },
  formula = formula, n = nrow(data), B = B
  )
  results <- list(estimates = estimates, formula = formula)
  class(results) <- "blblm"
  invisible(results)
}

#' Exact to par_blblm but makes us of chunking
#'
#' @param formula an lm formula
#' @param data the dataset containing the features mentioned in formula
#' @param m the number of subsamples the data is to be split into
#' @param B the number of bootstrap samples to take
#' @param cl this is equvalent to makeCluster and is created by the user outside the funtion.
#' @param ck the chunk size
#' @return list
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cl <- makeCluster(4)
#' par_blblm_chunk(mpg ~ hp, mtcars, 10, 100, cl, 2)
#' stopCluster(cl)
#' }
#'
par_blblm_chunk <- function(formula, data, m = 10, B = 5000, cl, ck) {
  data_list <- split_data(data, m)
  estimates <- parLapplyLB(cl, data_list, function(formula, data, n, B) { # added chunking option
    lm_each_subsample(formula = formula, data = data, n = nrow(data), B = B)
  },
  formula = formula, n = nrow(data), B = B, chunk.size = ck
  )
  results <- list(estimates = estimates, formula = formula)
  class(results) <- "blblm"
  invisible(results)
}


#' split data into m parts of approximated equal sizes
#'
#' @param data the data to be split
#' @param m the number of subsamples
split_data <- function(data, m) {
  idx <- sample_intC(m, nrow(data)) # rcpp sample_int
  data %>% split(idx)
}


#' compute the estimates
#'
#' @param formula a lm formula
#' @param data a data frame containing the features mentioned in formula
#' @param n the number of rows in the dataset
#' @param B the number of bootstrap samples to perform
#'
lm_each_subsample <- function(formula, data, n, B) {
  replicate(B, lm_each_boot(formula, data, n), simplify = FALSE)
}


#' compute the regression estimates for a blb dataset
#'
#' @param formula a lm formula
#' @param n the number of rows in the dataset
#' @param data a data frame containing the features mentioned in formula
#'
lm_each_boot <- function(formula, data, n) {
  freqs <- rmultinom(1, n, rep(1, nrow(data)))
  lm1C(formula, data, freqs) # changed to use lm1 written with C lm function
}

#' estimate the regression estimates using lm in rcpp
#'
#' @param formula a lm formula
#' @param data a data frame containing the features mentioned in formula
#' @param freqs the wights computed from a multinomial distribution in lm_each_boot
#'
lm1C <- function(formula, data, freqs) {
  environment(formula) <- environment()
  mf <- model.frame(formula, data) #creates compatible frame with wt_lm
  X <- model.matrix(formula, mf) #model matrix
  y <- model.response(mf) #the response
  fit <- wt_lm(y, X, freqs) #call rcpp lm
  list(formula = formula, coef = blbcoef(fit, formula), sigma = blbsigmaC(fit), stderr = fit$stderr)
}


#' compute the coefficients from fit
#'
#' @param fit the fitted model returned from blblm
#' @param formula the formula used to fit the model
#'
#' @export
blbcoef <- function(fit, formula) {
  cf <- coef(fit)
  parm <- attr(terms(formula), "term.labels") #pulls names
  labs <- c("int", parm) #adds intercept
  names(cf) <- labs #assigns names
  cf
}



#' Compute sigma from fit with lmC (compatible).
#' This functions is used for compatability with the rcpp version of lm.
#'
#' @param fit the fitted model returned from blblm
#'
blbsigmaC <- function(fit) {
  p <- fit$rank
  y <- fit$response
  e <- fit$fittedvals - y #added to account for fitted values calculated in cpp
  w <- fit$weights
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}

#' prints the model used in blblm
#'
#' @param x a blblm results
#' @param ... additional parameters
#'
#' @export
#' @method print blblm
print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}

#' calculates sigma from the blblm
#'
#' @param object a blblm result
#' @param confidence creates a confidence interval
#' @param level the level of confidence
#' @param ... additional parameters
#'
#' @export
#' @method sigma blblm
sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  if (confidence) {
    alpha <- 1 - 0.95
    limits <- est %>%
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}

#' pulls the coefficients from the blblm
#'
#' @param object a blblm result
#' @param ... additional parameters
#' @export
#' @method coef blblm
coef.blblm <- function(object, ...) {
  est <- object$estimates
  means <- map_mean(est, ~ map_rbind(., "coef") %>% colMeans())
  parm <- attr(terms(object$formula), "term.labels") #pull parameter names
  labs <- c("(intercept)", parm) #add intercept
  cf <- (means)
  names(cf) <- labs #assign names
  (cf)
}


#' @export
#' @method confint blblm
confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }


  alpha <- 1 - level
  est <- object$estimates
  out <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @title Predict
#' @description calculates a predicted value based on new data
#'
#' @param object a fitted model from blblm
#' @param new_data a data.frame with the values for the new data
#' @param confidence TRUE for a confidence interval of predicted value
#' @param level the level of confidence to use, default is .95
#' @param ... additional parameters
#' @return vector of predicted values and upper,lower bounds if confidence is TRUE
#'
#' @export
#' @method predict blblm
#' @examples
#' fit <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' predict(fit, data.frame(wt = c(2.5, 3), hp = c(150, 170)))
predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)

  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef[1, ]) %>% #access coef
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef[1, ]) %>% rowMeans()) #access coef
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
