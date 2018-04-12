## ----setup, echo=FALSE, results="hide"-----------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## ----install, results='asis', eval=FALSE---------------------------------
#  library(devtools)
#  devtools::install_github("gitter-lab/LPWC")

## ----lib, results="asis", eval=TRUE--------------------------------------
library(LPWC)

## ----data, results='markup'----------------------------------------------
data(simdata)
simdata[1:5, ]
str(simdata)

## ----time----------------------------------------------------------------
timepoints <- c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72)
timepoints

## ------------------------------------------------------------------------
LPWC::corr.bestlag(simdata[49:58, ], timepoints = timepoints, max.lag = 2, penalty = "high", iter = 10)

## ----clust1, fig.width=5-------------------------------------------------
dist <- 1 - LPWC::corr.bestlag(simdata[11:20, ], timepoints = timepoints, max.lag = 2, penalty = "low", iter = 10)$corr
plot(hclust(dist))

## ----clust 2-------------------------------------------------------------
dist <- 1 - LPWC::corr.bestlag(simdata[11:20, ], timepoints = timepoints, max.lag = 2, penalty = "low", iter = 10)$corr
cutree(hclust(dist), k = 3)

## ----sessionInfo---------------------------------------------------------
sessionInfo()

## ---- eval = FALSE-------------------------------------------------------
#  
#  
#  # This function stores two different list separately
#  comb <- function(x, ...) {
#    lapply(seq_along(x),
#           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
#  }
#  
#  
#  # adding the data
#  data <- simdata[1:10, ]
#  # number of iterations
#  iter <- 10
#  # C values that are used in the algorithm
#  allC <- findC(timepoints = timepoints, iter = iter)
#  
#  # setting the clusters
#  core <- parallel::detectCores() - 1
#  cl <- parallel::makeCluster(core)
#  
#  # assigning the parallelization
#  doParallel::registerDoParallel(cl)
#  
#  
#  ## running the algorithm for different C
#  result <- foreach(i = 1:iter, .combine='comb', .multicombine=TRUE,
#                  .init=list(list(), list())) %dopar% {
#              lags <- best.lag(data, max.lag = 3, timepoints = timepoints, C = allC[i])
#              new.data <- prep.data(data = data, lags = lags, timepoints = timepoints)
#              corr <- comp.corr(new.data$data, new.data$time, C = allC[i])
#              return(list(corr, lags))
#                  }
#  
#  
#  # dividing the list into two different list: one for lags and one for all the correlations
#  allcorr <- result[[1]]
#  alllags <- result[[2]]
#  
#  # picking best C
#  val <- rep(NA, (length(iter) - 1))
#  for(i in 1:(iter - 1)){
#    val[i] <- sum((as.vector(allcorr[[i + 1]]) - as.vector(allcorr[[i]]))^2)
#  }
#  
#  # returning the results for the best C
#  result <- list(corr = allcorr[[which.min(val) + 1]], lags = alllags[[which.min(val) + 1]], C = values[which.min(val) + 1])
#  

