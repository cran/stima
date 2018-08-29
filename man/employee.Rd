\name{employee}
\Rdversion{1.1}
\alias{employee}
\docType{data}
\title{
Employee Dataset
}
\description{
A dataset with information on background characteristics and salary of 473 employees.
}
\usage{data(employee)}
\format{
  A data frame with 473 observations on the following 9 variables:
  \describe{
    \item{\code{salary}}{a numeric variable, used as response variable: current salary in US dollars}
    \item{\code{age}}{a numeric variable: age in years}
    \item{\code{edu}}{a numeric variable: educational level in years}
    \item{\code{startsal}}{a numeric variable: beginning salary in US dollars}
    \item{\code{jobtime}}{a numeric variable: months since hire}
    \item{\code{prevexp}}{a numeric variable: previous work experience in months}
    \item{\code{minority}}{a factor variable: minority classification with levels \code{min}, indicating minority, and \code{no_min}, no minority}
    \item{\code{gender}}{a factor variable: gender type with levels \code{f}, indicating female, and \code{m}, indicating male}
    \item{\code{jobcat}}{a factor variable: type of job with levels \code{Clerical}, \code{Custodial}, and \code{manager}}
  }
}
\source{
This is an example dataset from the statistical software program SPSS, Version 20.0. If you use this dataset, refer to IBM Corp. (2011), see references. The dataset is used as a benchmark dataset in Dusseldorp, Conversano, and Van Os (2010).
}
\references{
IBM Corp. (2011). \emph{IBM SPSS Statistics for Windows, Version 20.0}. Armonk, NY: IBM Corp.


Dusseldorp, E. Conversano, C., and Van Os, B.J. (2010). Combining an additive and tree-based regression model simultaneously: STIMA. \emph{Journal of Computational and Graphical Statistics, 19(3)}, 514-530.
}

\keyword{datasets}
