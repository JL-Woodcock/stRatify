#' Obtain a cross validation fold from data based on an S\code{stratifiedXValS3} object.
#'
#' Returns a list of Training and Testing data based on folds which have been previously computed by \code{stratifySample()}.
#'
#' @param obj An object of type \code{StratifiedXValS3}.
#' @param fold The index of the fold you which to retrieve.
#' @param data The original data that the folds were computed on.
#'
#' @return A list containing two dataframes, \code{Train} and \code{Test}.
#'
#' @examples
#' predict()
#'
#' @export

predict.StratifiedXValS3 <- function(obj, fold, data) {

  outputList <- list(Train = data.frame(),
       Test = data.frame())

  df <- obj$Folds[,c("id", paste0("Fold_", fold))]

  TrainIds <- df[df[,paste0("Fold_", fold)] == "Train",]$id
  TestIds <- df[df[,paste0("Fold_", fold)] == "Test",]$id

  if (is.null(obj$idColname)) {
    warning("No unique column was given for sampling, original dataset must be ordered the same as this one.")
    outputList$Train <- data[TrainIds,]
    outputList$Test <- data[TestIds,]
  } else {

    outputList$Train <- data[data[, obj$idColname] %in% TrainIds,]
    outputList$Test <- data[data[, obj$idColname] %in% TestIds,]
  }

  return(outputList)

}

