\name{dendroclim}
\Rdversion{1.1}
\alias{dendroclim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of bootstrapped response and correlation functions.
}
\description{
This function calculates response and correlation functions from tree-ring chronologies and monthly climatic data. Function parameters are bootstrapped to calculate their significance and confidence intervals.
}
\usage{
dendroclim(chrono, clim, method = "response", start = -6, end = 9, timespan = NULL, vnames = NULL, sb = TRUE)
}
\arguments{
  \item{chrono}{
\code{data.frame} containing a tree-ring chronologies, e.g. as obtained by \code{chron} of package dplR.
}
  \item{clim}{
\code{data.frame} with climatic data in monthly resolution, with year, month and climate parameters in columns. All columns except year and month will be recognized as parameters for response or correlation function.
}
  \item{method}{
string specifying the calculation method. Possible values are \dQuote{response} and \dQuote{correlation}. Partial strings are ok.
}
  \item{start}{
integer value to determine the first month to be used as a predictor in the response or correlation function. A negative value denotes a start month from previous year, a positive value denotes a start month from current year.
}
  \item{end}{
integer value to determine the last month to be used as a predictor in the response or correlation function. A negative value denotes a end month from previous year, a positive value denotes a end month from current year.
}
  \item{timespan}{
integer vector of length 2 specifying the time interval (in years) to be considered for analysis. Defaults to the maximum possible interval.
}
  \item{vnames}{
character vector with variable names. defaults to corresponding column names of \code{data.frame} clim.
}
  \item{sb}{
logical flag indicating whether textual status bar should be suppressed. Suppression is recommended for e.g. Sweave files.
}
}
\details{
In its current state this function is a clone of programme DENDROCLIM2002 (Biondi and Waikul, 2004), and will calculate bootstrapped response and correlation functions in a similar manner as described in the above mentioned paper. In case of response function analysis 1000 bootstrap samples are taken from the original distribution and an eigen decomposition of the standardized predictor matrix is performed. Nonrelevant eigenvectors are removed using the PVP criterion (Guiot, 1990), principal component scores are then calculated from the matrices of reduced eigenvectors and standardized climatic predictors. Response coefficients are found via singular value decomposition, and tested for significance using the 95\% percentile range method (Dixon, 2001). In case of correlation function analysis, the coefficients are Pearson's correlation coefficients. The same method for significance testing is applied.

Input chronology data can be a \code{data.frame} such as produced by function \code{chron} of package dplR. It has to be a \code{data.frame} with at least one column containing the tree-ring indices, and the corresponding years as \code{rownames}. Input climatic data has to be a \code{data.frame} or \code{matrix} consisting of at least 3 rows for years, months and at least one climate parameter in the given order.

The window for response/correlation function analysis is specified via \code{start} and \code{end}, where e.g. -4 means previous April etc.
}
\value{
A \code{data.frame} with a response/correlation coefficient for each parameter, its significance (coded as 0/1) and its 95\% confidence intervall. 
}
\references{
Biondi, F. & Waikul, K. (2004) DENDROCLIM2002: A C++ program for statistical calibration of climate signals in tree-ring chronologies. \emph{Computers & Geosciences} 30:303-311

Dixon, P.M. (2001) Bootstrap resampling. In: El-Shaarawi, A.H., Piegorsch, W.W. (Eds.), \emph{The Encyclopedia of Environmetrics}. Wiley, New York.

Guiot, J. (1991) The boostrapped response function. \emph{Tree-Ring Bulletin} 51:39-41
}
\author{
Christian Zang
}

\seealso{
\code{\link{dcplot}}
}
\examples{
data(muc.clim) # climatic data
data(muc.spruce) # spruce data

# calculate and plot response function
dc.resp <- dendroclim(muc.spruce, muc.clim)
dcplot(dc.resp)

# calculate and plot correlation function
dc.corr <- dendroclim(muc.spruce, muc.clim, method = "corr")
dcplot(dc.corr)

# use modelled data for better response ;-)
data(muc.fake)
dc.resp.fake <- dendroclim(muc.fake, muc.clim)
dcplot(dc.resp.fake)
}

\keyword{ models }
\keyword{ multivariate }
\keyword{ nonparametric }
\keyword{ robust }