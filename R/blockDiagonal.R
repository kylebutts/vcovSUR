#' Given list of matrices, form block sparse matrix
#' 
#' @param mats List of sparse matrices `dgCMatrix`.
#' 
#' @return A block diagonal `dgCMatrix` with each element of mats
#' along the diagonal.
blockDiagonal <- function(mats) {
  mats = lapply(mats, function(mat) {
    if(inherits(mat, "matrix") | inherits(mat, "dgeMatrix")) {
      return(as(mat, "dgCMatrix"))
    } else {
      return(mat)
    }
  })

  nrows <- unlist(lapply(mats, nrow))
  cumsum_rows <- c(0, cumsum(nrows))
  ncols <- unlist(lapply(mats, ncol))
  nnzs <- c(0, unlist(lapply(mats, function(mat) length(mat@i))))
  cumsum_nnzs <- cumsum(nnzs)

  is = unlist(lapply(
    seq_along(mats), 
    function(idx) {
      mats[[idx]]@i + 1 + cumsum_rows[idx]
    }
  ))
  
  ps = unlist(lapply(
    seq_along(mats), 
    function(idx) {
      mats[[idx]]@p[-1] + cumsum_nnzs[idx]
    }
  ))
  ps = c(0, ps)

  xs = unlist(lapply(mats, function(m) m@x))
  
  Matrix::sparseMatrix(
    i = is, p = ps, x = xs,
    dims = c(sum(nrows), sum(ncols))
  )  
}


