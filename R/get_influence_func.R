#' Returns (X'X)^{-1} X'e from regression
#' 
#' @param est Estimation object of class `lm` or 
#'   `fixest` and `fixest_multi` from the `fixest` package
#' 
#' @return A matrix with rows consisting of (X'X)^{-1} X_i'e_i. 
#'   If `fixest_multi` is provided, then returns a 
#'   block diagonal matrix of influence functions.
#' 
get_influence_func <- function(est) {
  # (X'WX)^{-1} X'We
  if (inherits(est, "fixest")) {
    if (est$method == "feols") {
      XtWXinv = (est$cov.iid / est$sigma2)
    } else {
      XtWXinv = est$cov.iid
    }
    mat = XtWXinv %*% t(est$scores)
  } else { # code taken partially from `sandwich`
  
    X <- stats::model.matrix(est)
    if (any(alias <- is.na(stats::coef(est)))) X <- X[, !alias, drop = FALSE]
    if (!is.null(w <- stats::weights(est))) X <- X * sqrt(w)
    scores = X * stats::resid(est)
    mat = solve(crossprod(X), t(scores))
  }

  return(mat)
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
    
    cl = unlist(lapply(
      est, 
      function(x) { 
        df = fixest:::fetch_data(x)
        if (use_rowid) {
          1:nrow(df) 
        } else {
          df[[cluster]]
        }
      }
    ))

  } else if (inherits(est, "fixest")) {
    
    df = fixest:::fetch_data(est)

    # Note: don't use 1:est$nobs since some estimates might use different subsamples
    if (use_rowid) {
      cl = 1:nrow(df) 
    } else {
      cl = df[[cluster]]
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
    resid_dof = fixest::degrees_freedom(est, "resid")  
  } else if (inherits(est, "lm")) {
    est$df.residual
  }
}
