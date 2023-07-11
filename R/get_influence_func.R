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
  # (X'X)^{-1} X'e
  if (inherits(est, "fixest_multi")) {
    
    inf_list = lapply(est, function(x) { 
      (x$cov.iid / x$sigma2) %*% t(x$scores)
    })

    mat = blockDiagonal(inf_list)
    rownames(mat) = unlist(lapply(inf_list, rownames))

  } else if (inherits(est, "fixest")) {
    
    mat = (est$cov.iid / est$sigma2) %*% t(est$scores)
  
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
#' 
#' @return A vector of cluster labels
#' 
get_cluster <- function(est, cluster) {
  # (X'X)^{-1} X'e
  if (inherits(est, "fixest_multi")) {
    
    cl = unlist(lapply(
      est, 
      function(x) { 
        df = fixest:::fetch_data(x)
        df[[cluster]]
      }
    ))

  } else if (inherits(est, "fixest")) {
    
    df = fixest:::fetch_data(est)
    cl = df[[cluster]]
  
  } 
  
  return(cl)
}
