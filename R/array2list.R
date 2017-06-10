#' Reorganize an array of graphs to a list of graphs.
#'
#' \code{array2list} uses plyr to reshape an array of same-dimensional graphs
#' a list. Takes from array[,,length(listels)] to list[[listels]].
#'
#' @param array_out an array of dimensions [m x m x n].
#' @return list_in a list with n elements of dimensions [m x m].
#' @examples
#' test <- array(1:105, c(5, 7, 3))
#' array2list(test)  # has 3 elements with dimensions 5x7
#' @export
#' @seealso \code{\link{plyr}} \code{\link{list2array}}
#'
array2list <- function(list_in) {
  require(plyr)
  array_out <- alply(list_in, 3)
  return(array_out)
}
