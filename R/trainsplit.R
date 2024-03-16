#' Splits a dataframe into training and test sets.
#'
#'
#' @param data The data you want to split
#' @param trainpct Fraction of observations to go into training set. Must be >= 0 and =< 1. If set to 0 or 1, the empty test or training set will still inherit the same column names and types as the original dataset.
#' @param round_trainsize What to do when nrow(data) * trainpct is not a whole number. Default behavior is to round the size of the training set. Use 'ceiling' or 'floor' to instead set the size of training set to next highest or lowest whole number.
#' @param seed Sets the random seed; use this argument if you want to always get the same result. Note: sets seed only locally within the function.
#'
#' @return A list, containing the training and test sets.
#'
#' @examples
#' trainsplit(mtcars, trainpct = 0.75) # Returns a list containing the training and test sets
#' trainsplit(mtcars, trainpct = 0.001, round_trainsize = 'ceiling') # Size of training set rounds up to one
#' trainsplit(data.table::as.data.table(mtcars), trainpct = 0.75) # Also works with data.table

#' @export

trainsplit = function(data, trainpct = 0.75, round_trainsize = 'round', seed = NULL) {
   if (F) { # For debugging purposes
      data = iris
      data = as.data.table(iris)
      trainpct = 0.999
      round = 'ceiling'
      seed = 777
   }

   if (trainpct < 0 | trainpct >1) {
      stop('trainpct must be between 0 and 1')
   }

   if (!round_trainsize %in% c('round', 'floor', 'ceiling')) {
      stop('round_trainsize must be "round", "floor", or "ceiling"')
   }

   if (round_trainsize == 'ceiling') {
      trainsize = ceiling(trainpct*nrow(data))
   } else if (round_trainsize == 'floor') {
      trainsize = floor(trainpct*nrow(data))
   } else {
      trainsize = round(trainpct*nrow(data), digits = 0)
   }

   output <- list()
   if (!is.null(seed)) {
      set.seed(seed)
   }

   index_train <- sample.int(n = nrow(data), size = trainsize, replace = F)

   if ('data.table' %in% class(data)) {
      require(data.table)
      if (trainsize == 0) {
         output$train = copy(data[integer(0), ])
         output$test = copy(data)
      } else if (trainsize == nrow(data)) {
         output$train = copy(data)
         output$test = copy(data[integer(0), ])
      } else {
         output$train <- copy(data[index_train, ])
         output$test <- copy(data[-index_train, ])
      }
   } else {
      if (trainsize == 0) {
         output$train = data[integer(0), ]
         output$test = data
      } else if (trainsize == nrow(data)) {
         output$train = data
         output$test = data[integer(0), ]
      } else {
         output$train <- data[index_train, ]
         output$test <- data[-index_train, ]
      }
   }

   stopifnot(nrow(output$train)+nrow(output$test) == nrow(data))
   return(output)
}

