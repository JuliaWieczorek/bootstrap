#' @title A jackknife function ver. extended
#'
#' @description The jackknife is a method used to estimate the variance
#' and bias of a large population.
#' This was the earliest resampling method, even before bootstrap method.
#' It involves a leave-one-out strategy of the estimation of a parameter
#' (e.g., the mean) in a data set of N observations (or records).
#' This function additionally compute standard error and bias
#'
#' @param X data vector
#' @param ES estimation function, np. mean, var, sd, median or function created by you
#'
#' @keywords jackknife, bootstrap
#'
#' @return standard error, bias and selected estimator
#'
#' @examples
#' x = c(138.5, 138.5, 140.0, 141.0, 141.0, 143.5, 145.0, 147.0, 148.5, 150.0, 153.0, 154.0, 155.0, 156.5, 157.0, 158.5, 159.0, 159.0, 159.0, 160.5, 161.0, 162.0)
#' jackknife(x, mean)
#'
#' @export
#'

standard.error <- function(x) {
  sd(x)/sqrt(length(x))
}

jackknife2 <- function(X, ES) {
  mi <- mean(X)
  n <- length(X)
  ests <- numeric(n)
  for (i in 1:n)
    ests[i] <- ES(X[-i])
  se = standard.error(ests)
  bias <- mean(ests)- mi
  structure(list("bład standardowy" = se, "obciążenie estymatora średniej" = bias, "estymator" = ests))}


