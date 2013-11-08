\name{prune.rt}
\alias{prune.rt}
\title{
Pruning of a regression trunk.
}
\description{
Determines the optimally pruned size of the regression trunk by applying the \emph{c}*standard error rule to the results from the cross-validation procedure.
}
\usage{
\method{prune}{rt}(tree, data, c.par = NULL,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
a tree of class \code{rt}, that is, a regression trunk. This is the result of \code{\link{stima}} using the option \code{model="regtrunk"}. To be able to prune, it is a prerequisite that the cross-validation procedure was performed with \code{\link{stima}}.
}
  \item{data}{
the dataset that was used to create the regression trunk.
}
  \item{c.par}{
the pruning parameter (\emph{c}) that will be used in the \emph{c*SE} rule. In the default option, the pruning function uses the best value of \emph{c}, as recommended by Dusseldorp, Conversano & van Os (2010). This best value depends on the sample size of the included dataset.
}
 \item{\dots}{
additional arguments to be passed.  
}
}
\value{
The function returns the pruned regression trunk, and the corresponding regression trunk model. The output is an object of class \code{rt}. If the pruning rule resulted in the root node, no object is returned. 
}
\references{

Dusseldorp, E. Conversano, C., and Os, B.J. (2010). Combining an additive and tree-based regression model simultaneously: STIMA. \emph{Journal of Computational and Graphical Statistics, 19(3)}, 514-530.
}
\seealso{
 \code{\link{stima},\link{summary.rt}}
}
 \examples{
#Example with employee data
data(employee)
#a regression trunk with a maximum of three splits is grown
#variable used for the first split (edu) is third variable in the dataset
#twofold cross-validation is performed to save time in the example,
#tenfold cross-validation is recommended
 
emprt1<-stima(employee,3,first=3,vfold=2)
summary(emprt1)
#prune the regression trunk
emprt1_pr<-prune(emprt1,data=employee)

}


\keyword{ prune }

