library(Matrix)
library(fixest)

test_that("influence function works", {
  est1 = feols(mpg ~ wt + i(cyl), mtcars)

  X <- stats::model.matrix(est1)
  e <- stats::resid(est1)
  inf_manual <- solve(crossprod(X), t(X * e))
  inf <- get_influence_func(est1)

  expect_equal(inf, inf_manual)
})

test_that("influence function works with weights", {
  mtcars$w = runif(nrow(mtcars), 0.15, 0.85)
  est_w = feols(mpg ~ wt + i(cyl), mtcars, weights = ~ w)

  bread = est_w$cov.iid / est_w$sigma2
  scores = est_w$scores

  expect_equal(
    tcrossprod(bread %*% t(scores)), 
    vcov(est_w, "hc1", ssc = ssc(adj = FALSE, cluster.adj = FALSE))
  )
})
