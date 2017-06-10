#' Reorganize a list of graphs to an array of graphs
#'
#' \code{list2array} uses abind to reshape a list of same-dimension graphs to
#' an array. Takes from list[[listels]] to array[,,length(listels)].
#'
#' @param list_in a list with n elements of dimensions [m x m].
#' @return array_out an array of dimensions [m x m x n].
#' @examples
#' test <- list()
#' test[[1]] <- matrix(1:35, nrow=5, ncol=7)
#' test[[2]] <- matrix(36:70, nrow=5, ncol=7)
#' test[[3]] <- matrix(71:105, nrow=5, ncol=7)
#' list2array(test)  # has dimensions [5, 7, 3]
#' @export
#' @seealso \code{\link{abind}} \code{\link{array2list}}
#'
list2array <- function(list_in) {
  require(abind)
  array_out <- do.call(abind, c(list_in, list(along=3)))
  return(array_out)
}
