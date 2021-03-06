% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap2.R
\name{bootstrap2}
\alias{bootstrap2}
\title{A bootstrap function}
\usage{
bootstrap2(X, n, ES)
}
\arguments{
\item{X}{data vector}

\item{n}{number of bootstrap repetitions}

\item{ES}{estimation function, np. mean, var, sd or function create by you}
}
\value{
The selected estimator
}
\description{
This functions allows you to estimate the value of the estimator
by means of multiple draws with returning from a sample.
}
\examples{
x = c(138.5, 138.5, 140.0, 141.0, 141.0, 143.5, 145.0, 147.0, 148.5, 150.0, 153.0, 154.0, 155.0, 156.5, 157.0, 158.5, 159.0, 159.0, 159.0, 160.5, 161.0, 162.0)
bootstrap(x, 200, mean)
bootstrap(x, 500, mean)
bootstrap(x, 1000, mean)
bootstrap(x, 10000, mean)

}
\keyword{bootstrap,}
\keyword{sample}
