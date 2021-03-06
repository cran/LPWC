#' @title Computing corr
#'
#' @description This function computes the weighted correlation with a penalty for lags.
#' It should only be used after the fixed lags have already been applied to the dataset and timepoints using the functions
#' prep.data() and best.lag().
#'
#'
#' @param data a lagged matrix or data frame with rows representing genes and columns representing
#' different timepoints (NAs added when lags are needed)
#' @param time a lagged matrix with rows representing each gene's timepoint and columns representing the number of timepoints, NA is introduced when it is lagged
#' @param C a numeric value of C used in computing weighted correlation
#' @return a simmilarity matrix with values between -1 and 1
#' (1 highly correlated, 0 no correlation)
#'
#' @importFrom stats as.dist
#'
#' @examples
#' ## This function computes the correlation after the lags (or shifts) have
#' ## been computed.  In this example, the lags argument is randomly sampled
#' ## for the sake of illustrating how prep.data() applies the lags and
#' ## prepares a transformed dataset for comp.corr().
#' lagged <- prep.data(array(rnorm(30), c(3, 10)), timepoints = seq(0, 45, 5),
#'           lags = sample(c(0, 1, -1, 2, -2), size = 3))
#' comp.corr(data = lagged$data, time = lagged$time, C = 10)
#'
#' ## This example shows how comp.corr is used in practice with real data.
#' ## The best.lag() function is called first to pre-compute the lags, which
#' ## are passed to prep.data().
#' randdata <- array(rnorm(120), c(10, 12))
#' bl <- best.lag(data = randdata, timepoints = 1:12, C = 5)
#' lag.data <- prep.data(randdata, timepoints = 1:12, lags = bl)
#' comp.corr(lag.data$data, time = lag.data$time, C = 5)
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#' @export comp.corr
#'


comp.corr <- function(data, time, C){
  if(!is.null(row.names(data))){rownames <- row.names(data)}
  else{rownames <- NULL}
  #checking for all the conditions with data, time and C
  stopifnot(all(dim(data) == dim(time)), is.numeric(C), C > 0)
  #creating an empty matrix to store the correlation values
  corr <- array(NA, c(dim(data)[1], dim(data)[1]))
  #iterating through each i and j to print the correlation value
  for(j in 1:(dim(data)[1] - 1)){
    for(i in (j + 1):dim(data)[1]){
      #alligning the peptides and timepoints
      pair <- weight.lag(data[i, !is.na(data[i, ])], data[j, !is.na(data[j, ])])
      times <- weight.lag(time[i, !is.na(time[i, ])], time[j, !is.na(time[j, ])])
      #computing w0 and w from the alliged timepoints
      weights <- apply(times, 2, function(x){(diff(x)) ^ 2})
      #computing correlation from the values
      corr[i, j] <- exp(-1 / C * mean(weights)) * wt.corr(pair[1, ], pair[2, ], exp(-1 / C * weights))
    }
  }
  rownames(corr) <- rownames
  #picking the correlation matrix
  return(as.dist(corr))
}


