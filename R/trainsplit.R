#' Splits a dataframe into training and test sets.
#'
#'
#' @param data The data you want to split
#' @param trainpct Fraction of observations to go into training set. Must be > 0 and < 1. NOTE: This function "rounds up", so that at least 1 observation will go into the training and test sets.
#' @param return If "list", returns list containing the training and test set. If "train" or "test", returns only the training or test sets.
#' @param seed Sets the random seed. Note: sets seed only locally within the function.
#' 
#' @return A dataframe containing either the test set, the training set, or a list containing both.
#' @export


trainsplit = function(data, trainpct = 0.75, return = 'list', seed = 111) {
  # data = mtcars
  # trainpct = 0

  old <- .Random.seed
  set.seed(seed)
  if (trainpct <= 0 | trainpct >=1) {
    stop('trainpct must be > 0 and < 1')
  }
  trainsize <- max(1, floor(trainpct*nrow(data))) # There's always at least 1 obs in train/test
  index_train <- sample.int(n = nrow(data), size = trainsize, replace = F)
  output <- list()
  output$train <- data[index_train, ]
  output$test <- data[-index_train, ]

  stopifnot(nrow(output$train)+nrow(output$test) == nrow(data))
  .Random.seed <<- old

  if (return == 'train') {
    return(output$train)
  }
  if (return == 'test') {
    return(output$test)
  }
  return(output)
}


