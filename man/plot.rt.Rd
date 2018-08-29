\name{plot.rt}
\Rdversion{1.1}
\alias{plot.rt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to plot a regression trunk.
}
\description{
Results in a plot a regression trunk.
}
\usage{
\method{plot}{rt}(x,digits=2,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{rt}, typically the result of \code{\link{stima}} using the option \code{model="regtrunk"}.}
\item{digits}{number of decimal places used in the plot. Default value is 2.}
  \item{\dots}{
additional arguments to be passed.  
}
}

\details{
  The output is a plot of a regression trunk. Exception: If the first splitting predictor is categorical with more than 2 categories, the output will be multiple plots: for each category one plot of a regression trunk.
 }

\note{ The number of digits of the mean \eqn{y} value displayed in each node can be adjusted using the command \code{options(digits =..)} before the plot command.

Known bug: If a splitting variable (not the first one) in the regression trunk is categorical, the values of the categories are not displayed in the plot.  }
 
 \seealso{  \code{\link{stima},\link{stima.control},\link{summary.rt}}} 
\examples{

data(employee)
fit1<-stima(employee,2,first=3,vfold=0)

##adjust the number of decimal places used in the plot
plot(fit1,digits=1)

#categorical first split 
fit2<-stima(employee,3,first=9,vfold=0)
plot(fit2)    
#click on the plot to see the next one 
#for each category of variable "jobcat" the subtree is shown in a separate plot

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~plot  }

