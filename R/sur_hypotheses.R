

#' Hypothesis testing from seemingly-unrelated regressions
#' 
#' @description This function takes a set of regression esimates and 
#'   perform hypothesis tests using `vcovSUR` to calculate the 
#'   variance-covariance matrix between regressions. The underlying 
#'   hypothesis test is conducted using `marginaleffects::hypotheses`. 
#'   Since the same regressors can be used in multiple regressions, the
#'   coefficients will be suffixed with `_#` with the `#` corresponding to
#'   the estimates position in the `ests` object.
#' 
#' @inheritParams vcovSUR
#' @param hypothesis String containing the hypothesis to test.
#'   See `marginaleffects::hypotheses` for more details. Additionally, 
#'   see the description for more details to specify which regression
#'   a coefficient is from in the hypothesis.
#' @inheritParams marginaleffects::hypotheses
#' 
#' 
#' @export
sur_hypotheses <- function(ests, cluster = NULL, hypothesis = NULL, conf_level = 0.95, df = NULL, joint = FALSE, joint_test = "f") {

  ests = split_fixest_list(ests)
  
  coeftable <- lapply(
    seq_along(ests), 
    function(i) {
      coef <- stats::coef(ests[[i]])
      tab <- data.frame(
        term = paste0(names(coef), "_", i),
        estimate = as.numeric(coef)
      )
    }
  )
  coeftable <- do.call("rbind", coeftable)
  class(coeftable) <- c("sur_model", "data.frame")

  vcov_sur <- vcovSUR(ests, cluster = cluster)
  colnames(vcov_sur) = rownames(vcov_sur) = coeftable$term

  marginaleffects::hypotheses(
    coeftable, hypothesis = hypothesis,
    vcov = vcov_sur,
    FUN = function(x) { x },
    conf_level = conf_level, 
    joint = joint, 
    joint_test = joint_test
  )
}


get_coef.sur_model = function(model) {
  x = model$estimate
  names(x) = model$term
  return(x)
}

set_coef.sur_model = function(model, coefs) {
  model$estimate = coefs
  return(model)
}



