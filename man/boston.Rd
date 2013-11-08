\name{boston}
\Rdversion{1.1}
\alias{boston}
\docType{data}
\title{
Boston house-price data 
}
\description{
The response is the median value of owner-occupied homes measured for each of 506 cencus tracts in the Boston area.

}
\usage{data(boston)}
\format{
  A data frame with 506 observations on the following 16 variables.
  \describe{
    \item{\code{c.medv}}{numeric response variable: median value of owner-occupied homes measured in 1000's USD}
    \item{\code{chas}}{a factor with levels \code{"lontano"} and \code{"vicino"}, indicating if a suburb tracts the bound of Charles river (= "lontano") or not}
    \item{\code{long}}{a numeric variable: longitude}
    \item{\code{latid}}{a numeric variable: latitude of census tract}
    \item{\code{crim}}{a numeric variable: per capita crime rate per town}
    \item{\code{zn}}{a numeric variable: proportion of residential land zoned for lots over 25,000 sq.ft.}
    \item{\code{indus}}{a numeric variable: proportion of non-retail business acres per town}
    \item{\code{nox}}{a numeric variable: nitric oxides concentration (parts per 10 million)}
    \item{\code{rm}}{a numeric variable: average number of rooms per dwelling}
    \item{\code{age}}{a numeric variable: proportion of owner-occupied units built prior to 1940}
    \item{\code{dis}}{a numeric variable: weighted distances to five Boston employment centers}
    \item{\code{rad}}{a numeric variable: index of accessibility to radial highways}
    \item{\code{tax}}{a numeric variable: full-value property-tax rate per 10,000 USD}
    \item{\code{ptratio}}{a numeric variable: pupil-teacher ratio by town}
    \item{\code{b}}{a numeric variable: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town}
    \item{\code{lstat}}{a numeric variable: percentage lower status of the population}
  }
}
\source{
Statlib website: http://lib.stat.cmu.edu/datasets
}
\references{
Harrison, D. and Rubinfeld, D.L. (1978). Hedonic prices and the demand for clean air. \emph{J. Environ. Economics & Management, 5,} 81-102.

}

\keyword{datasets}
