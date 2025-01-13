#' @title trainsplit
#' @description Splits a dataframe, tibble, or data.table into a test set and training set. Specify either the number or percentage of observations to be put into training set.
#'
#' @param data The dataset you want to split
#' @param ntrain The number of observations to go into the training set. Must be >= 0 and <= nrow(data).
#' @param trainpct Fraction of observations to go into training set. Must be >= 0 and =< 1. If set to 0 or 1, the empty test or training set will still inherit the same column names and types as the original dataset.
#' @param round_ntrain What to do when nrow(data) * trainpct is not a whole number. Default behavior is to round the size of the training set. Use 'ceiling' or 'floor' to instead set the size of training set to next highest or lowest whole number.
#' @param seed Sets the random seed; use this argument if you want to always get the same result. Note: sets seed only locally within the function.
#' @param return Three return modes available: "parentenv" assigns the training and test sets into the environment that called the function with names based on the name of the original dataset; this is intended largely for an educational context. "list" will return a list with the training and test sets. "index" will return only the numerical index of the rows to be placed into the training set, which can then be manually subset by the user.
#'
#' @return Depends on "return" argument; either a list, an index, or NULL if return = "parentenv" was selected.
#'
#' @examples
#' # Splits the training and test sets and assigns them into memory.
#' trainsplit(mtcars, trainpct = 0.75)
#' # Specify size of training set by number of rows, not percent:
#' trainsplit(mtcars, ntrain = 10)
#' # Size of training set rounds to one:
#' trainsplit(mtcars, trainpct = 0.01, round_ntrain = 'ceiling')
#' # Also works with data.table:
#' trainsplit(data.table::as.data.table(mtcars), trainpct = 0.75)
#' # Return a list containing the training/test sets instead:
#' trainsplit(mtcars, trainpct = 0.75, return = 'list')

#' @export
#' @importFrom data.table copy as.data.table

trainsplit = function(data, ntrain = NULL, trainpct = NULL,
                      round_ntrain = 'round', seed = NULL,
                      return = 'parentenv') {

   if (!is.null(seed)) {
      set.seed(seed)
   }
   if (is.null(ntrain) & is.null(trainpct)) {
      stop('Must specify either ntrain or trainpct.')
   }
   if (!is.null(ntrain) & !is.null(trainpct)) {
      stop('Must specify either ntrain or trainpct, but not both')
   }

   # Determine ntrain based on trainpct
   if (!is.null(trainpct)) {
      if (trainpct <= 0 | trainpct >= 1) {
         stop('trainpct must be between 0 and 1')
      }
      if (identical(round_ntrain,'ceiling')) {
         ntrain = ceiling(trainpct*nrow(data))
      } else if (identical(round_ntrain,'floor')) {
         ntrain = floor(trainpct*nrow(data))
      } else if (identical(round_ntrain, 'round')) {
         ntrain = round(trainpct*nrow(data), digits = 0)
      } else {
        stop('round_ntrain must be "round", "floor", or "ceiling"')
      }
   }

   if (ntrain < 0 | ntrain > nrow(data)) {
      stop('ntrain must be between 0 and nrow(data)')
   }

   # Split data
   index_train <- sample.int(n = nrow(data), size = ntrain, replace = F)
   if (ntrain == 0) {
      train = data[integer(0), ]
      test = data
   } else if (ntrain == nrow(data)) {
      train = data
      test = data[integer(0), ]
   } else {
      train <- data[index_train, ]
      test <- data[-index_train, ]
   }
   rownames(train) = NULL
   rownames(test) = NULL
   stopifnot(nrow(train) + nrow(test) == nrow(data))

   # Return modes
   if (identical(return, 'parentenv')) {
      origname = deparse(substitute(data))
      if ('data.table' %in% class(data)) { # Prevents shallow copy warning when using predict() after split
         assign(paste0(origname, '_train'), copy(train), pos = parent.frame())
         assign(paste0(origname, '_test'), copy(test), pos = parent.frame())
      } else {
         assign(paste0(origname, '_train'), train, pos = parent.frame())
         assign(paste0(origname, '_test'), test, pos = parent.frame())
      }
      return(invisible(NULL))
   }

   if (identical(return,'list')) {
      return(list(train = train, test = test))
   }

   if (identical(return,'index')) {
      return(index_train)
   }

   stop('"return" must be "parentenv", "list", or "index"')
}

