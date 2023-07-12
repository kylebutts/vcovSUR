library(fixest)
library(Matrix)

#' Get joint variance-covariance matrix of seemingly unrelated regressions
#' 
#' @param ests List of `feols` estimates.
#' @param cluster Scalar string denoting the name of the cluster column.
#' @param df Scalar string denoting which degrees of freedom adjustment to 
#'   make. Default is null which uses `n`. Can be either NULL, `"dfk"` or 
#'   `"dfk2"` following Stata options. See details.
#' 
#' @details 
#' `dfk` and `dfk2` specifies the use of an alternative divisor in computing 
#'   the covariance matrix for the equation residuals. As an asymptotically 
#'   justified estimator, the default uses the number of sample observations 
#'   (n) as a divisor when the two datasets have the same number of 
#'   observations. 
#' 
#' When the `dfk` option is set, a small-sample adjustment is 
#'   made and the divisor is taken to be the square root of (n - k_i)(n - k_j), 
#'   where k_i and k_j are the number of parameters in equations i and j, 
#'   respectively (similarly the cube root of the product is taken if there are 
#'   three estimates and so on..). 
#' 
#' When the `dfk2` option is set, the divisor is taken to be the mean of the 
#'   residual degrees of freedom from the individual equations.
#' 
#' @return Variance-covariance matrix of the combined set of coefficients in 
#' all models in `ests`.
#' 
#' @export
vcovSUR <- function(ests, cluster = NULL, df = NULL) {

  ests = split_fixest_list(ests)
  inf_list <- lapply(ests, get_influence_func)
  inf_funcs <- block_diagonal(inf_list)
  col_names = unlist(lapply(inf_list, rownames))

  # Figure our what to do with degree of freedom adjustment
  # dofs <- unlist(lapply(ests, get_dof))
  # prod(dofs)^(1 / length(dofs))

  if (!is.null(cluster)) {
    cl = unlist(lapply(
      ests, 
      function(x) get_cluster(x, cluster)
    ))

    group_col_idx = split(seq_along(cl), as.factor(cl))
    inf_funcs = lapply(group_col_idx, function(idx) {
      Matrix::rowSums(inf_funcs[, idx, drop = FALSE])
    })
    inf_funcs = do.call("cbind", inf_funcs)
  }

  vcov = Matrix::tcrossprod(inf_funcs)
  vcov = as.matrix(vcov)
  rownames(vcov) = colnames(vcov) = col_names
  return(vcov)
}

