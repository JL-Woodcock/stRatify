testData <- load("data/smbsimdf1.RData")
library(dplyr)

stratifySample <- function(data, targetVariable, nfolds = 5, uniqueID = NULL, seed = 123,
                           positiveResponse = 1, keepData = FALSE) {

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
    df[ ,fold + 2] <- ifelse(df$id %in% xValList[["Train"]][[fold]]$id, "Train", ifelse(df$id %in% xValList[["Test"]][[fold]]$id, "Test", "Error"))
    colnames(df)[fold + 2] <- paste0("Fold_", fold)
  }

  outObject <- list(Folds = df,
                    Full_Data = ifelse(keepData == TRUE, cbind(data, df[, 3:nfolds + 2]), NA),
                    idColname = uniqueID)

  class(outObject) <- "StratifiedXValS3"
  outObject

}


stratifySample(data = smbsimdf1 %>% filter(rnd < 0.84), "fgood", nfolds = 3)
l <- stratifySample(data = smbsimdf1, "fgood", nfolds = 7)






