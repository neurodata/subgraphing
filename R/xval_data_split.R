#' Cross-Validation Data Splitter
#'
#' \code{sg.bern.xval_split_data} Bernoulli Subgraph Classifier with Cross Validation to determine the optimal subgraph.
#'
#' @param samp [n x n x s] an array of input data.
#' @param Y [s] the class labels.
#' @param holdout [z] the examples to hold out for the validation set.
#' @return train_set [n x n x (s - z)] the training examples.
#' @return train_y [(s - z)] the labels for the training set.
#' @return test_set [n x n x s] the testing examples.
#' @return test_y [s] the labels for the testing set.
#' @export
#'
sg.xval_split_data <- function(samp, Y, holdout) {
  test_set <- samp[,,holdout]
  test_y <- Y[holdout]

  all_ex <- 1:dim(samp)[3]  # a sequence of all the samples
  train_exs <- setdiff(all_ex, holdout)
  train_set <- samp[,,train_exs]
  train_y <- Y[train_exs]

  return(list(train_set=train_set, train_y=train_y,
              test_set=test_set, test_y=test_y))
}
