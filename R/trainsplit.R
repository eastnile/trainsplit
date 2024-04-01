#' @title trainsplit
#' @description Splits a dataframe, tibble, or data.table into a test set and training set. Specify either the number or percentage of observations to put into training set.
#'
#' @param data The dataset you want to split
#' @param ntrain The number of observations to go into the training set. Must be >= 0 and <= nrow(data).
#' @param trainpct Fraction of observations to go into training set. Must be >= 0 and =< 1. If set to 0 or 1, the empty test or training set will still inherit the same column names and types as the original dataset.
#' @param round_ntrain What to do when nrow(data) * trainpct is not a whole number. Default behavior is to round the size of the training set. Use 'ceiling' or 'floor' to instead set the size of training set to next highest or lowest whole number.
#' @param seed Sets the random seed; use this argument if you want to always get the same result. Note: sets seed only locally within the function.
#'
#' @return A list, containing the training and test sets.
#'
#' @examples
#' # Returns a list containing the training and test sets:
#' trainsplit(mtcars, trainpct = 0.75)
#' # Specify size of training set by number of rows, not percent:
#' trainsplit(mtcars, ntrain = 10)
#' # Size of training set rounds to one:
#' trainsplit(mtcars, trainpct = 0.01, round_ntrain = 'ceiling')
#' # Also works with data.table:
#' trainsplit(data.table::as.data.table(mtcars), trainpct = 0.75)

#' @export
#' @importFrom data.table copy as.data.table

trainsplit = function(data, ntrain = NULL, trainpct = NULL,
                      round_ntrain = 'round', seed = NULL) {

   if (is.null(ntrain) & is.null(trainpct)) {
      stop('Must specify desired size or percentage of data to go into training set.')
   }

   if (!is.null(ntrain) & !is.null(trainpct)) {
      stop('Must specify either ntrain or trainpct, but not both')
   }

   if (!is.null(ntrain)) {
      if (ntrain <= 0 | ntrain >= nrow(data)) {
         stop('ntrain must be between 0 and nrow(data)')
      }
   }
   if (!is.null(trainpct)) {
      if (trainpct <= 0 | trainpct >= 1) {
         stop('trainpct must be between 0 and 1')
      }
      if (!round_ntrain %in% c('round', 'floor', 'ceiling')) {
         stop('round_ntrain must be "round", "floor", or "ceiling"')
      }
      if (round_ntrain == 'ceiling') {
         ntrain = ceiling(trainpct*nrow(data))
      } else if (round_ntrain == 'floor') {
         ntrain = floor(trainpct*nrow(data))
      } else {
         ntrain = round(trainpct*nrow(data), digits = 0)
      }
   }

   if (!is.null(seed)) {
      set.seed(seed)
   }

   output <- list()
   index_train <- sample.int(n = nrow(data), size = ntrain, replace = F)

   if ('data.table' %in% class(data)) {
      if (ntrain == 0) {
         output$train = data.table::copy(data[integer(0), ]) # keep structure of data even if train or test set is empty
         output$test = data.table::copy(data) # Copy() needed to prevent reference error
      } else if (ntrain == nrow(data)) {
         output$train = data.table::copy(data)
         output$test = data.table::copy(data[integer(0), ])
      } else {
         output$train <- data.table::copy(data[index_train, ])
         output$test <- data.table::copy(data[-index_train, ])
      }
   } else {
      if (ntrain == 0) {
         output$train = data[integer(0), ]
         output$test = data
      } else if (ntrain == nrow(data)) {
         output$train = data
         output$test = data[integer(0), ]
      } else {
         output$train <- data[index_train, ]
         output$test <- data[-index_train, ]
      }
   }

   rownames(output$train) = NULL
   rownames(output$test) = NULL
   stopifnot(nrow(output$train)+nrow(output$test) == nrow(data))
   return(output)
}

