#' @title bootstrapKNN
#'
#' @description Based on multiple sampling, it calculates kNN statistics and compares them with the kNN statistics
#' calculated from the original sample.
#'
#' @param X imput data (matrix)
#' @param len length of the first vector
#' @param n number of bootstrap repetitions
#' @param neighbour the number of nearest neighbors i.e. determining the value of k
#'
#' @keywords kNN, bootstrap
#'
#' @importFrom FNN
#' @importFrom kNN_stat
#'
#' @return p_value
#'
#' @examples
#' X <- matrix(rnorm(15, mean=5, sd=sqrt(3)), 5, 5)
#' Y <- matrix(rnorm(15, mean=5, sd=sqrt(3)), 6, 5)
#' macierz <- rbind(X, Y)
#' o <- rep(0, nrow(macierz4))
#' dane <- data.frame(macierz4, o)
#' dane
#' w <- length(X)
#' library(FNN)
#' bootKNN(dane, w, 10, nrow(dane)-1)
#'
#' @export
#'


bootKNN <- function(X, len, n, neighbour){
  N1 <- kNN_stat(X, neighbour, len)
  boot <- lapply(1:n, function(i) sample(as.matrix(X), replace = T))
  n.test <- sapply(boot, function(i) kNN_stat(as.matrix(i), neighbour, len))
  cnt <- 0
  for (i in 1:length(n.test)){
    if(n.test[i]>=N1){cnt=cnt+1}
  }
  p_value <- (cnt+1)/(n+1)
  return((p_value))}
