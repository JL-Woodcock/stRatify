#' Performs stratified cross validation sampling of a data frame.
#'
#' Generates cross validation folds in a stratified way.
#'
#' @param data A \code{dataframe} which contains the base data to be split.
#' @param targetVariable A \code{string} giving the name of the binary variable to stratify by.
#' @param nfolds An integer giving the number of cross validation folds.
#' @param uniqueID A \code{string} giving the name of a unique row identifier. Note, if there is no identifier, the function
#' will filter the dataset by index.
#' @param seed The seed value to use to reproduce results.
#' @param positiveResponse The value of the target variable which indicates a positive response. Must be 1 or 0.
#' @param keepData A Boolean indicating whether or not to keep the entire dataset along with extra columns indicating the folds.
#'
#' @return An object of the class \code{StratifiedXValS3}.
#'
#' @examples
#' stratifySample()
#'
#' @export

stratifySample <- function(data, targetVariable, nfolds = 5, uniqueID = NULL, keepData = FALSE, positiveResponse = 1, seed = NULL) {

  # Validate inputs.
  if (!is.data.frame(data)) {
    stop("Data is not of type data.frame.")
  }

  if (!is.integer(nfolds) | nfolds <= 1) {
    stop("Argument nfolds must be a positive integer greater than or equal to two.")
  }

  if (!is.null(uniqueID)) {

    if (is.null(data[[uniqueID]])) {
      stop("uniqueID column does not exist in data.")
    }

    if (length(unique(data[[uniqueID]])) != nrow(data)) {
      stop("Given unique ID is not unique on data.")
    }

  }

  if (is.null(data[[targetVariable]])) {
    stop("Target variable does not exist in data frame.")
  }

  if (sort(unique(data[[targetVariable]])) != c(0,1)) {
    stop("Target variable must be binary numeric and have both values represented.")
  }

  negativeResponse <- (1 - positiveResponse)

  # Create an ID column if one isn't given.
  if (is.null(uniqueID)) {
    idColumn <- seq(1:nrow(data))
  } else {
    idColumn <- data[[uniqueID]]
  }


  # Create dataframe with just target and ID column.
  df <- data.frame(id = idColumn,
                   target = data[[targetVariable]])

  responsePos <- df %>%
    dplyr::filter(target == positiveResponse) %>%
    dplyr::mutate(rand = runif(nrow(.))) %>%
    dplyr::arrange(rand) %>%
    dplyr::select(-rand)

  responseNeg <- df %>%
    dplyr::filter(target == negativeResponse) %>%
    dplyr::mutate(rand = runif(nrow(.))) %>%
    dplyr::arrange(rand) %>%
    dplyr::select(-rand)

  posList <- list()
  negList <- list()

  for (fold in seq(1:nfolds)) {

    startNeg <- floor(((fold - 1) * nrow(responseNeg))/nfolds) + 1
    endNeg <- floor(((fold) * nrow(responseNeg))/nfolds)

    startPos <- floor(((fold - 1) * nrow(responsePos))/nfolds) + 1
    endPos <- floor(((fold) * nrow(responsePos))/nfolds)

    posList[[fold]] <- responsePos[startPos: endPos, ]
    negList[[fold]] <- responseNeg[startNeg: endNeg, ]

  }

  permBase <- seq(1:nfolds)

  testList <- list()
  trainList <- list()

  for (testElement in permBase) {

    trainElements <- permBase[permBase != testElement]

    trainDf <- data.frame()
    for (i in trainElements) {
      trainDf <- rbind(trainDf, posList[[i]], negList[[i]])
    }

    testList[[testElement]] <- rbind(posList[[testElement]], negList[[testElement]])
    trainList[[testElement]] <- trainDf

      }

  xValList <- list(Train = trainList, Test = testList)

  for (fold in 1:nfolds) {
    df[ ,fold + 2] <- ifelse(df$id %in% xValList[["Train"]][[fold]]$id,
                             "Train", ifelse(df$id %in% xValList[["Test"]][[fold]]$id, "Test", "Error"))
    colnames(df)[fold + 2] <- paste0("Fold_", fold)
  }

  outObject <- list(Folds = df,
                    Full_Data = ifelse(keepData == TRUE, cbind(data, df[, 3:nfolds + 2]), NA),
                    idColname = uniqueID)

  class(outObject) <- "StratifiedXValS3"
  outObject

}




