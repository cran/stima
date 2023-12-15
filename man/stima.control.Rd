\name{stima.control}
\Rdversion{1.1}
\alias{stima.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Control options for the stima function 
}
\description{
The output are various parameters that control aspects of the simultaneaous threshold interaction algorithm
}
\usage{
stima.control(minbucket = NULL, crit = "f2", mincrit = 0.001, 
predtrunk = NULL, ref = 1, sel = "none", ksel = 2, predsel = NULL, 
cvvec = NULL, seed = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{minbucket}{the minimum number of observations in a terminal node. The default is the square root of the total sample size.
}
  \item{crit}{
the type of statistic to be used in the partitioning criterion. The default for the regression trunk model is the effect size \code{"f2"} which equals the relative increase in variance accounted for. Other options are \code{"R2change"} which is the absolute increase in variance accounted for, or \code{"F-value"} which is the \emph{F}-statistic of the anova test.
}
  \item{mincrit}{
the minimum node deviance before growing stops.
}
  \item{predtrunk}{
a row vector that indicates the column numbers in the data frame of the predictors that can be used in the regression trunk. The default action uses all predictors as available splitting candidates; NB. this column number can not be 1, because the first column is the response variable.
}
  \item{ref}{ a number referring to the region of the regression trunk that will be used as reference category in the regression trunk model. The default value is 1, referring to \emph{R1}. }
 \item{sel}{ if \code{sel = "backward"}, the full regression trunk model is reduced using a backward selection procedure; if \code{sel = "manual"}, one needs to give a specification of predsel. }
  \item{ksel}{ the multiple of the number of degrees of freedom used for the penalty in the backward selection procedure. The default value is 2, which gives the genuine AIC: \code{ksel = log(n)} is sometimes referred to as BIC or SBC. }
  \item{predsel}{row vector that indicates the column numbers in the \code{newdata} set (obtained by \code{Save = TRUE} in \code{\link{stima}}) of the predictors to be used in the final regression trunk model.}
 \item{cvvec}{ index vector for the rows of the dataframe that will be used in each cross-validation set. The default option is a random division into \code{"vfold"} sets. }
   \item{seed}{an integer between 0 and 1023 that will be used in set.seed(). The default value equals 3. }
}

\value{
a list containing the parameters.
}
\references{
Dusseldorp, E. Conversano, C., and Van Os, B.J. (2010). Combining an additive and tree-based regression model simultaneously: STIMA. \emph{Journal of Computational and Graphical Statistics, 19(3)}, 514-530.
}

\seealso{  \code{\link{stima},\link{summary.rt},\link{plot.rt},\link{prune.rt}}}
\examples{

##Adjust the stopping rule in a minimum of 5 observations in a terminal node
data(employee)
contr1<-stima.control(minbucket=5)


##Adjust the seed used to create an index vector for the 10fold cross-validation 
##With seed=3, the result equals the one reported in the online Appendix D of  
##the paper in the Journal of Computational and Graphical Statistics
##NB. To save time in the example, the splitting candidates of the regression  
##trunk(i.e., edu and jobtime) are selected with predtrunk=c(3,5),
##where 3 and 5 denote the column numbers in the dataset

contr2<-stima.control(sel="backward",seed=3,predtrunk=c(3,5))
emprt2<-stima(employee,2,first=3,control=contr2)
summary(emprt2)


##Apply a manual selection of predictors to be used in the pruned model
 
contr3<-stima.control(sel="manual",predsel=c(2,3,4,5,6,8))

 }


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ control-options}

