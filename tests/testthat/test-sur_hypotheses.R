test_that("sur_hypotheses works", {
  mtcars$id = 1:nrow(mtcars)
  est1 <- feols(mpg ~ wt, mtcars)
  est2 <- feols(mpg ~ wt + i(cyl), mtcars)

  hyp_test = sur_hypotheses(
    list(est1, est2), cluster = NULL,
    hypothesis = "wt_1 = wt_2"
  )

  expect_equal(
    as.numeric(hyp_test$estimate),
    coef(est1)[["wt"]] - coef(est2)[["wt"]]
  )

  hyp_test_clustered = sur_hypotheses(
    list(est1, est2), cluster = "id",
    hypothesis = "wt_1 = wt_2"
  )

  expect_true(
    as.numeric(hyp_test_clustered$p.value) != as.numeric(hyp_test$p.value)
  )
})

test_that("sur_hypotehses fixest_multi works", {
  ests <- feols(c(mpg, hp) ~ wt, mtcars)

  hyp_test = sur_hypotheses(
    ests, hypothesis = "wt_1 = wt_2"
  )

  expect_equal(
    as.numeric(hyp_test$estimate),
    coef(ests[[1]])[["wt"]] - coef(ests[[2]])[["wt"]]
  )
})
