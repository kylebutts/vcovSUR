library(fixest)
library(Matrix)

#' Get joint variance-covariance matrix of seemingly unrelated regressions
#' 
#' @param ests List of `feols` estimates.
#' @param cluster Scalar string denoting the name of the cluster column.
#' 
#' @return Variance-covariance matrix of the combined set of coefficients in 
#' all models in `ests`.
#' 
#' @export
vcovSUR <- function(ests, cluster = NULL) {
  if (inherits(ests, "fixest") | inherits(ests, "fixest_multi") | inherits(ests, "lm")) { 
    ests = list(ests)
  } 

  inf_list <- lapply(ests, get_influence_func)
  inf_funcs <- blockDiagonal(inf_list)
  col_names = unlist(lapply(inf_list, rownames))

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

