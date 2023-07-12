library(Matrix)

test_that("block_diagonal works", {
  mat1 <- Matrix::rsparsematrix(32, 2, density = 0.5)
  mat2 <- as.matrix(Matrix::rsparsematrix(32, 5, density = 0.5))
  mat3 <- Matrix::rsparsematrix(32, 3, density = 0.5)

  # Test: `block_diagonal` produces the correct matrix
  block_manual <- rbind(
    cbind(mat1, Matrix::Matrix(0, nrow = nrow(mat1), ncol = ncol(mat2))),
    cbind(Matrix::Matrix(0, nrow = nrow(mat2), ncol = ncol(mat1)), mat2)
  )
  block <- block_diagonal(list(mat1, mat2))
  expect_true(all(block == block_manual))

  block_manual_3 <- rbind(
    cbind(
      mat1, Matrix::Matrix(0, nrow = nrow(mat1), ncol = ncol(mat2) + ncol(mat3))
    ),
    cbind(
      Matrix::Matrix(0, nrow = nrow(mat2), ncol = ncol(mat1)), mat2, Matrix::Matrix(0, nrow = nrow(mat2), ncol = ncol(mat3))
    ),
    cbind(
      Matrix::Matrix(0, nrow = nrow(mat3), ncol = ncol(mat1) + ncol(mat2)), mat3
    )
  )
  block_3 <- block_diagonal(list(mat1, mat2, mat3))

  expect_true(all(block_3 == block_manual_3))
})
