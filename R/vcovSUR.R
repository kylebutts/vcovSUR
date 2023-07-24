library(fixest)
library(Matrix)

#' Get joint variance-covariance matrix of seemingly unrelated regressions
#' 
#' @param ests List of `feols` estimates.
#' @param cluster Scalar string denoting the name of the cluster column. If 
#'   the same dataset is used (or subsets across samples) and the cluster 
#'   variable "rowid" is passed, this will use the row-number of the original
#'   data. This with `ssadj = TRUE` will obtain the original standard errors.
#' @param ssadj Logical indicating whether to apply a small-sample adjustment. 
#'   See details for how the adjustment is made.
#' 
#' @details 
#' The small-sample adjustment depends on the `cluster` option and the `ssadj`  
#'   option. For each estimate, the degree of freedom calculation will be 
#'   calculated. Denote them as c_i. For block V_ij, the adjustment will 
#'   be sqrt(c_i * c_j) so that the diagonal elements are adjusted by c_i. 
#'   This matches `dfk` option in `sureg`. If `cluster` is used (and is not 
#'   "rowid"), then the adjustment will be G/(G-1) where G is the  
#'   number of unique clusters. If `cluster` is `NULL` or is `"rowid"`, then 
#'   the adjustment will be (N_i - 1)(N_i - K_i) where N_i is the 
#'   number of observations and K_i is the number of coefficients used in 
#'   estimate i. 
#' 
#' If a `fixest_multi` object is used and `cluster` is not specified, then 
#'   `cluster` will automatically be set to `"rowid"` since the same dataset
#'   is used in all estimates.
#' 
#' @return Variance-covariance matrix of the combined set of coefficients in 
#' all models in `ests`.
#' 
#' @export
vcovSUR <- function(ests, cluster = NULL, ssadj = FALSE) {

  use_rowid = 
    (!is.null(cluster) && cluster == "rowid") | 
    (is.null(cluster) & inherits(ests, "fixest_multi"))

  ests = split_fixest_list(ests)
  inf_list <- lapply(ests, function(x) t(inf_func(x)))
  inf_funcs <- block_diagonal(inf_list)
  col_names = unlist(lapply(inf_list, rownames))
  ks <- unlist(lapply(inf_list, function(x) nrow(x)))
  
  if (!is.null(cluster)) {
    cls = lapply(
      ests, 
      function(x) get_cluster(x, cluster, use_rowid)
    )

    # DOF adjustment when clustering (not by "rowid")
    adjs = unlist(lapply(cls, function(x) {
      G = length(unique(x))
      return(G / (G - 1))
    }))
    cl = unlist(cls)

    group_col_idx = split(seq_along(cl), as.factor(cl))
    inf_funcs = lapply(group_col_idx, function(idx) {
      Matrix::rowSums(inf_funcs[, idx, drop = FALSE])
    })
    inf_funcs = do.call("cbind", inf_funcs)
  } 
  
  if (use_rowid | is.null(cluster)) {
    # DOF adjustment when not clustering (or clustering by "rowid")
    # (n-1) / (n-k)
    adjs = unlist(lapply(
      ests, function(x) {
        (x$nobs - 1) / fixest::degrees_freedom(x, "resid")
      }
    ))
  }

  vcov = Matrix::tcrossprod(inf_funcs)
  vcov = as.matrix(vcov)
  rownames(vcov) = colnames(vcov) = col_names
  if (ssadj) vcov = apply_adjs(vcov, ks, adjs)
  return(vcov)
}

apply_adjs <- function(vcov, ks, adjs) { 
  split_idx = c(1, cumsum(ks) + 1)

  for (i in 1:length(ks)) { 
    for (j in 1:length(ks)) { 
      sel_i = split_idx[i]:(split_idx[i + 1] - 1)
      sel_j = split_idx[j]:(split_idx[j + 1] - 1)
      adj_ij = vcov[sel_i, sel_j] * sqrt(adjs[i] * adjs[j])

      # Store
      vcov[sel_i, sel_j] = adj_ij
      vcov[sel_j, sel_i] = adj_ij
    }
  }

  return(vcov)
}

