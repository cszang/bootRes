\name{mdcc}

%% <LIST>
\alias{mdcc}
%% </LIST>

\Rdversion{1.1}
\alias{mdcc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Moving Response and Correlation Functions.
}
\description{
This function calculates moving response and correlation functions from
tree-ring chronologies and monthly climatic data. The calculation is
performed repeatedly for consecutive time windows. Function parameters
may be bootstrapped to calculate their significance and confidence
intervals.
}
\usage{
mdcc(chrono, clim, method = "response", start = 4, end = 9, timespan =
NULL, vnames = NULL, sb = TRUE, win.size = 25, win.offset =
1, startlast = TRUE, boot = FALSE, ci = 0.05)
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
integer value to determine the last month to be used as a predictor in the response or correlation function. A negative value denotes an end month from previous year, a positive value denotes an end month from current year.
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
\item{win.size}{
  integer giving the window size for each recalculation.
}
\item{win.offset}{
integer giving the number of years between each window start.
}
\item{startlast}{
logical flag indicating whether the first window should start at the
rear end (youngest part of the series) or not.
}
\item{boot}{
logical flag indicating whether bootstrap resampling is to be
performed.
}
\item{ci}{
numerical value to set the test level for significance test (values
0.01, 0.05 and 0.1 are allowed); the confidence intervals are adapted accordingly.}
}
\details{

The functions \code{\link{dcc}} and \code{\link{mdcc}} clone the
functionality of programme DENDROCLIM2002 (Biondi and Waikul, 2004), and
will calculate bootstrapped (and non-bootstrapped) moving (\code{mdcc}
and static (\code{dcc}) response and correlation functions in a similar
fashion as described in the above mentioned paper.

In case of response function analysis 1000 bootstrap samples are taken
from the original distribution and an eigen decomposition of the
standardized predictor matrix is performed. Nonrelevant eigenvectors are
removed using the PVP criterion (Guiot, 1990), principal component
scores are then calculated from the matrices of reduced eigenvectors and
standardized climatic predictors. Response coefficients are found via
singular value decomposition, and tested for significance using the 95\%
percentile range method (Dixon, 2001). In case of correlation function
analysis, the coefficients are Pearson's correlation coefficients. The
same method for significance testing is applied.

Input chronology data can be a \code{data.frame} such as produced by
function \code{chron} of package dplR. It has to be a \code{data.frame}
with at least one column containing the tree-ring indices, and the
corresponding years as \code{rownames}.

For climatic input data, there are three possibilities: Firstly, input
climatic data can be a \code{data.frame} or \code{matrix} consisting of
at least 3 rows for years, months and at least one climate parameter in
the given order. Secondly, input climatic data can be a single
\code{data.frame} or \code{matrix} in the style of the original
DENDROCLIM2002 input data, i.e. one parameter with 12 months in one row,
where the first column represents the year. Or thirdly, input climatic
data can be a list of several of the latter described \code{data.frame}
or \code{matrices}. As an internal format dispatcher checks the format
automatically, it is absolutely necessary that in all three cases, only
complete years (months 1-12) are provided. It is not possible to mix
different formats in one go.

The window for response/correlation function analysis is specified via
\code{start} and \code{end}, where e.g. -4 means previous April etc.

The window size for moving response and correlation functions is set via
\code{win.size}, and the distance from one window start to the next is
set with the parameter \code{win.offset}. Parameter \code{startlast}
indicates, wether the first window is started from the rear (youngest
part) of the series or not.

Bootstrapping (\code{boot}) is by default disabled to get the results
faster.

}
\value{
  A \code{list} containing \code{data.frame}s for coefficients and
  confidence intervals for each parameter and time window used for the
  moving functions.
}
\references{

  Biondi, F. & Waikul, K. (2004) DENDROCLIM2002: A C++ program for
  statistical calibration of climate signals in tree-ring
  chronologies. \emph{Computers & Geosciences} 30:303-311

  Dixon, P.M. (2001) Bootstrap resampling. In: El-Shaarawi, A.H.,
  Piegorsch, W.W. (Eds.), \emph{The Encyclopedia of
  Environmetrics}. Wiley, New York.

  Guiot, J. (1991) The boostrapped response function. \emph{Tree-Ring
    Bulletin} 51:39-41
  
}
\author{
Christian Zang
}

\seealso{
  \code{\link{mdcplot}}
  \code{\link{dcc}}
}
\examples{
data(muc.clim) # climatic data
data(muc.spruce) # spruce data

# calculate and plot moving response function
dc.mov1 <- mdcc(muc.spruce, muc.clim)
mdcplot(dc.mov1)

# calculate and plot moving correlation function with different window
# parameters

data(rt.spruce)
data(rt.temp)
data(rt.prec)

dc.mov2 <- mdcc(rt.spruce, list(rt.temp, rt.prec),
                vnames = c("temp", "prec"),
                method = "corr", win.size = 20,
win.offset = 5)
mdcplot(dc.mov2)
}

\keyword{models}
\keyword{multivariate}
\keyword{nonparametric}
\keyword{robust}