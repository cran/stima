\name{stima}
\Rdversion{1.1}
\alias{stima}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simultaneous Threshold Interaction Modeling Algorithm
}
\description{
This function fits a regression trunk model (default option) using the simultaneous threshold interaction modeling algorithm.
The algorithm fits a regression tree and a multiple regression model simultaneously.
}
\usage{
stima(data, maxsplit, model = "regtrunk", first = NULL, vfold = 10, 
CV = 1, Save = FALSE, control = NULL, printoutput = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a data frame with one continuous response variable and multiple predictors (categorical or continuous). IMPORTANT: The first column is treated as the response variable, the remaining columns as predictors.
}
  \item{maxsplit}{
the maximum number of splits.
}
  \item{model}{
the default model is a regression trunk model. The classification trunk model is under development.
}
  \item{first}{
the column number in the data frame of the predictor that is used for the first split of the regression trunk. The default option automatically selects the predictor for the first split.
}
 \item{vfold}{the number of sets to be used in the cross-validation. The default value is 10, which means 10-fold cross-validation. If \code{vfold = 0}, no cross-validation is performed. }
  \item{CV}{the number of times the cross-validation procedure is performed. The default is once. If \code{CV = 5} and \code{vfold = 10}, five times a tenfold cross-validation is performed. }
  \item{Save}{if \code{Save = TRUE}, the new data are saved and added to the output of the \code{rt}-object. The data include indicator variables of the terminal nodes (regions) of the regression trunk. }
  \item{control}{
options controlling details of the algorithm. For default options see \code{\link{stima.control}}.
}
  \item{printoutput}{
if TRUE, output will be printed while running the function.
}
}

\value{
an object of class \code{rt}, which is a list containing at least the following components 
    \item{call}{the matched call.}
    \item{trunk}{the fitted regression trunk. MeanResponse is the mean response value of the observations in that particular node (this is not the predicted response value).}
    \item{splitsequence}{the number of the nodes that are split.}
    \item{goffull}{goodness-of-fit estimates of the full regression trunk model estimated after 1 split through the model estimated after the maximum number of splits.}
    \item{full}{the estimated full regression trunk model after the maximum number of splits. Coefficient = estimated unstandardized regression coefficient; Std. Coef. = standardized regression coefficient. }
}
\references{
Dusseldorp, E. & Meulman, J. J. (2004). The regression trunk approach to discover treatment covariate interaction. \emph{Psychometrika, 69,} 355-374.

Dusseldorp, E. Conversano, C., and Van Os, B.J. (2010). Combining an additive and tree-based regression model simultaneously: STIMA. \emph{Journal of Computational and Graphical Statistics, 19(3)}, 514-530.

}

\seealso{
 \code{\link{stima.control},\link{summary.rt},\link{prune.rt},\link{plot.rt} } and \code{help("stima-package")}
}
\examples{


#Example with Boston Housing dataset from paper in JCGS
data(boston)
#grow a full regression trunk with automatic first split selection 
#and maximum number of splits = 10, with: bostonrt<-stima(boston,10)  
#NB. This analysis will take a long time (about one hour)
#inspect the output with: summary(bostonrt)
#prune the tree with: prune(bostonrt,data=boston)
#the pruned regression trunk has 7 splits
#to save time in the example, we select the splitting candidates beforehand,
#and we grow a tree with a maximum of 4 splits: 
contr<-stima.control(predtrunk=c(8,9,16)) 
bostonrt_pr<-stima(boston,4,first=16,vfold=0,Save=TRUE,control = contr) 
summary(bostonrt_pr)
#inspect the coefficients of the final regression trunk model
round(bostonrt_pr$full,digits=2)
#inspect the new data including the indicator variables referring 
#to the terminal nodes
bostonrt_pr$newdata
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ regression }
\keyword{ interaction }% __ONLY ONE__ keyword per line
\keyword{ tree }
