#' Split fixest_multi into seperate elements of a list
#' @export
split_fixest_list <- function(ests) {
  if (inherits(ests, "fixest") | inherits(ests, "fixest_multi") | inherits(ests, "lm")) {
    ests <- list(ests)
  }

  res <- list()
  for (i in seq_along(ests)) {
    est <- ests[[i]]
    if (inherits(est, "fixest_multi")) {
      res <- c(res, lapply(est, function(x) x))
    } else {
      res <- c(res, list(est))
    }
  }
  names(res) <- NULL
  return(res)
}
