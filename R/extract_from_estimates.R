#' Extract vector of empirical influence function from regression model
#'
#' Extracts the empirical influence function from a model
#'   \url{https://en.wikipedia.org/wiki/Robust_statistics#Empirical_influence_function}.
#'   This is useful for residaul bootstrap and for cross-model inference.
#'   In linear models, influence function is given by: (X' W X)^-1 X' W^1/2 y.
#'
#' @title inf_func
#' @param est model object
#' @param ... other arguments
#' @return A N x k matrix of influence function where N is the number of 
#'   observations and k is the number of moments (parameters in the linear 
#'   case).
#' @examples
#'   mod <- fixest::feols(mpg ~ wt, data = mtcars)
#'   inf_func(mod)
#' @export
inf_func <- function(est, ...) {
  UseMethod("inf_func")
}

#' @rdname inf_func
#' @export
inf_func.lm <- function(est, ...) {
  # code taken partially from `sandwich`
  X <- stats::model.matrix(est)
  if (any(alias <- is.na(stats::coef(est)))) X <- X[, !alias, drop = FALSE]
  if (!is.null(w <- stats::weights(est))) X <- X * sqrt(w)
  scores <- X * stats::resid(est)
  
  t(solve(crossprod(X), t(scores)))
}

#' @rdname inf_func
#' @export
inf_func.fixest <- function(est, ...) {
  if (est$method == "feols") {
    XtWXinv <- (est$cov.iid / est$sigma2)
  } else {
    XtWXinv <- est$cov.iid
  }
  
  t(XtWXinv %*% t(est$scores))
}

#' Returns (X'X)^{-1} X'e from regression
#'
#' @param est Estimation object of class `fixest`
#'   and `fixest_multi` from the `fixest` package
#' @param cluster Scalar string denoting the name
#'   of the cluster column.
#' @param use_rowid Scalar logical denoting if row number
#'   should be used as the cluster.
#'
#' @return A vector of cluster labels
#'
get_cluster <- function(est, cluster, use_rowid) {
  # (X'X)^{-1} X'e
  if (inherits(est, "fixest_multi")) {
    cl <- unlist(lapply(
      est,
      function(x) {
        df <- fixest:::fetch_data(x)
        if (use_rowid) {
          1:nrow(df)
        } else {
          df[[cluster]]
        }
      }
    ))
  } else if (inherits(est, "fixest")) {
    df <- fixest:::fetch_data(est)

    # Note: don't use 1:est$nobs since some estimates might use different subsamples
    if (use_rowid) {
      cl <- 1:nrow(df)
    } else {
      cl <- df[[cluster]]
    }
  }

  return(cl)
}


#' Returns small-sample adjustment
#'
#' @param est Estimation object of class `fixest`
#'   and `fixest_multi` from the `fixest` package
#' @param cluster Scalar string denoting the name
#'   of the cluster column.
#'
#' @return A vector of degrees of freedoms
#'
get_adjs <- function(est, cluster = NULL) {
  if (inherits(est, "fixest")) {
    resid_dof <- fixest::degrees_freedom(est, "resid")
  } else if (inherits(est, "lm")) {
    est$df.residual
  }
}
