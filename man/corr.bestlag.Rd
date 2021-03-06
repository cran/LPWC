% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corr_bestlag.R
\name{corr.bestlag}
\alias{corr.bestlag}
\title{Computes best lag correlation}
\usage{
corr.bestlag(data, timepoints, max.lag = NULL, C = NULL,
  penalty = "high", iter = 10)
}
\arguments{
\item{data}{a matrix or data frame with rows representing genes and columns
representing different timepoints. If data is a data frame, the gene names
can be specified using the \code{row.names()}.}

\item{timepoints}{a vector of time points used in the dataset}

\item{max.lag}{a integer value of the maximum lags allowed in the dataset,
if null, defaults to the floor of the number of timepoints divided by 4}

\item{C}{a numeric value of C used in computing weighted correlation,
if null, a default is computed based on the penalty argument}

\item{penalty}{a factor with two levels high and low penalty on the weighted correlation}

\item{iter}{an integer indicating the number of C values to test for low penalty}
}
\value{
a list containing weighted correlation and best lags used in each row
}
\description{
This function computes correlation based on best picked lags. The lags indicate delayed changes.
}
\examples{
corr.bestlag(array(rnorm(30), c(5, 6)), max.lag = 1,
          timepoints = c(0, 5, 10, 15, 20, 25), C = 10, penalty = "high")
corr.bestlag(array(runif(40, 0, 20), c(4, 10)),
          timepoints = c(0, 0.5, 1.5, 3, 6, 12, 18, 26, 39, 50), penalty = "high")
corr.bestlag(matrix(data = rexp(n = 40, 2), nrow = 8),
          timepoints = c(0, 5, 15, 20, 40), penalty = "low", iter = 5)

}
\author{
Thevaa Chandereng, Anthony Gitter
}
