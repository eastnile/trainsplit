#' Splits a dataframe into training and test sets.
#'
#'
#' @param data The data you want to split
#' @param trainpct Fraction of observations to go into training set. Must be >= 0 and =< 1.
#' @param return If "list", returns list containing the training and test set. If "train" or "test", returns only the training or test sets.
#' @param round What to do when nrow(data) * trainpct is not a whole number. Default is to round down the size of the training set. Set to 'ceiling' to round up.
#' @param seed Sets the random seed. Note: sets seed only locally within the function.
#' 
#' @return A dataframe containing either the test set, the training set, or a list containing both.
#' @examples 
#' trainsplit(mtcars, trainpct = 0.75, return = 'train') # Returns the training set
#' trainsplit(mtcars, trainpct = 0.01, return = 'train', round = 'floor') # size of training set rounds down to zero
#' trainsplit(mtcars, trainpct = 0.01, return = 'train', round = 'ceiling') # size of training set rounds up to one.
#' @export


trainsplit = function(data, trainpct = 0.75, return = 'list', round = 'floor', seed = 111) {
  
  # data = iris
  # trainpct = 0.999
  # return = 'list'
  # round = 'ceiling'
  # seed = 111
  
  old_seed <- .Random.seed # save old random seed
  set.seed(seed)
  if (trainpct <= 0 | trainpct >=1) {
    stop('trainpct must be between 0 and 1')
  }
  
  if (!return %in% c('list', 'train', 'test')) {
    stop('return must be "list", "train", or "test"')
  }
  
  if (!round %in% c('floor', 'ceiling')) {
    stop('round must be "floor" or "ceiling"')
  }
  
  if (round == 'ceiling') {
    trainsize = ceiling(trainpct*nrow(data))
  } else {
    trainsize = floor(trainpct*nrow(data))
  }
  
  output <- list()
  index_train <- sample.int(n = nrow(data), size = trainsize, replace = F)
  
  if (trainsize == 0) {
    output$train = data[NULL,]
    output$test = data
  } else if (trainsize == nrow(data)) {
    output$train = data
    output$test = data[NULL,]
  } else {
    output$train <- data[index_train, ]
    output$test <- data[-index_train, ]
  }

  stopifnot(nrow(output$train)+nrow(output$test) == nrow(data))
  .Random.seed <<- old_seed # restore old random seed

  if (return == 'train') {
    return(output$train)
  }
  if (return == 'test') {
    return(output$test)
  }
  return(output)
}


