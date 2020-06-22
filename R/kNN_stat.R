#' @title kNN statistics
#'
#' @description The FNN::knn function identifies the k-nearest neighbors using Euclidean distance
#' where k is a user-specified number.
#'
#' @param data imput data (matrix)
#' @param neighbour number of nearest neighbors i.e. determining the value of k
#' @param len length of the first vector
#'
#' @keywords kNN
#'
#' @importFrom FNN
#'
#' @return statistic of k-Nearest Neighbors - KNN
#'
#' @examples
#' matrix <- rbind(c(0.00, 2.21, 2.49, 1.78, 3.11, 2.94),
#' c(2.21, 0.00, 2.60, 3.27, 1.45, 3.29),
#' c(2.49, 2.60, 0.00, 2.14, 0.43, 3.52),
#' c(1.78, 3.27, 2.14, 0.00, 2.69, 2.60),
#' c(3.11, 1.45, 0.43, 2.69, 0.00, 3.48),
#' c(2.94, 3.29, 3.52, 2.60, 3.48, 0.00))
#' colnames(matrix) <- c('X1', 'X2', 'X3', 'Y1', 'Y2', 'Y3')
#' rownames(matrix) <- c('X1', 'X2', 'X3', 'Y1', 'Y2', 'Y3')
#' dane <- data.frame(matrix)
#' dane
#' value <- nrow(dane)/2
#' library(FNN)
#' kNN_stat(matrix, 3, value)
#'
#' @export
#'

kNN_stat <- function(data, neighbour, len){
  NS <- get.knn(data, k=neighbour)
  index <- NS$nn.index
  block1 <- NS$nn.index[1:3, ]
  block2 <- NS$nn.index[4:6, ]
  i1 <- sum(block1 <= len)
  i2 <- sum(block2 > len)
  vector <- c(i1, i2)
  stat <- sum(vector)/neighbour*nrow(data)
  return(stat)
}

