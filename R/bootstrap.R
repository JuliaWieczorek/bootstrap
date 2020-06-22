#' @title A bootstrap function
#'
#' @description This functions allows you to estimate the value of the estimator
#' by means of multiple draws with returning from a sample.
#'
#' @param X data vector
#' @param n number of bootstrap repetitions
#' @param ES estimation function, np. mean, var, sd or function create by you
#'
#' @keywords bootstrap
#'
#' @return The selected estimator
#'
#' @examples
#' x = c(138.5, 138.5, 140.0, 141.0, 141.0, 143.5, 145.0, 147.0, 148.5, 150.0, 153.0, 154.0, 155.0, 156.5, 157.0, 158.5, 159.0, 159.0, 159.0, 160.5, 161.0, 162.0)
#' bootstrap(x, 200, mean)
#' bootstrap(x, 500, mean)
#' bootstrap(x, 1000, mean)
#' bootstrap(x, 10000, mean)
#'
#' @export


bootstrap <- function(X, n, ES){
  mi <- mean(X)
  boot <- lapply(1:n, function(i) sample(X, replace = T))
  e <- sapply(boot, ES)
  return (e)
}

