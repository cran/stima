\name{summary.rt}
\Rdversion{1.1}
\alias{summary.rt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarizing Regression Trunk Model Fits from stima
}
\description{
\code{summary} method for class \dQuote{rt} (i.e. a regression trunk)
}
\usage{
\method{summary}{rt}(object, digits = 3,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class \code{rt}, usually a result of a call to \code{\link{stima}} using the default option:\code{model="regtrunk"} 
}
  \item{digits}{
the number of decimals to used in the output.
}
\item{\dots}{
Additional arguments to be passed  
}
}

\value{
The function \code{summary.rt} returns the goodness-of-fit summary of the estimated regression trunk model, using the components \dQuote{\code{goffull}} and, if available, \dQuote{\code{gofsel}}.
   \item{full}{goodness-of-fit estimates of the full regression trunk model estimated after 1 split through the model estimated after the maximum number of splits. \code{f2} = the effect size of the indicator variable added to the model after a split. \code{RE} = apparent error; \code{SE} = standard error of RE; \code{REcv} = cross-validated error; \code{SEcv} = standard error of REcv. If available: \code{REcvm} = Average cross-validated error; \code{SEcvm} = standard error of REcvm.}
   \item{selected}{goodness-of-fit estimates of the selected regression trunk model (if applicable).}
}

\seealso{
\code{\link{stima.control},\link{stima},\link{plot.rt} }
}
