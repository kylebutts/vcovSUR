library(fixest)

test_that("Basic Usage", {

  mtcars$id = 1:nrow(mtcars)
  est1 <- feols(
    c(mpg, wt) ~ hp + i(cyl), 
    mtcars, cluster = ~id
  )
  vcov_joint = vcovSUR(est1, cluster = "id")
  se_joint = vcov_joint |>
    diag() |> sqrt()
  se_sep = c(
    se(est1[[1]], ssc = ssc(adj = FALSE, cluster.adj = FALSE)), 
    se(est1[[2]], ssc = ssc(adj = FALSE, cluster.adj = FALSE))
  )

  testthat::expect_equal(se_joint, se_sep)

})

test_that("Weights", {

  mtcars$id = 1:nrow(mtcars)
  mtcars$w = runif(nrow(mtcars), 0.1, 0.9)
  est2 <- feols(
    c(mpg, wt) ~ hp + i(cyl), 
    mtcars, cluster = ~id, weights = ~wt
  )
  vcov_joint = vcovSUR(est2, cluster = "id")
  se_joint = vcov_joint |>
    diag() |> sqrt()
  se_sep = c(
    se(est2[[1]], ssc = ssc(adj = FALSE, cluster.adj = FALSE)), 
    se(est2[[2]], ssc = ssc(adj = FALSE, cluster.adj = FALSE))
  )

  testthat::expect_equal(se_joint, se_sep)

})

test_that("Different Data Sets", {
  est1 <- feols(
    mpg ~ 0 + wt + i(vs), mtcars[mtcars$gear == 3, ]
  )
  est2 <- feols(
    mpg ~ 0 + wt + i(vs), mtcars[mtcars$gear == 4, ]
  )
  est3 <- feols(
    mpg ~ 0 + wt + i(vs), mtcars[mtcars$gear == 5, ]
  )
  vcov_sur = vcovSUR(list(est1, est2, est3))

  # Off diagonal == 0
  expect_true(
    all(vcov_sur[4:9, 1:3] == 0) & 
    all(vcov_sur[c(1:3, 7:9), 4:6] == 0) & 
    all(vcov_sur[1:6, 7:9] == 0)
  )
})

