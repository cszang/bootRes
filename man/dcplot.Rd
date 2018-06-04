\name{dcplot}
\Rdversion{1.1}
\alias{dcplot}
\title{
Plotting Function for Response and Correlation Functions
}
\description{
A simple plotting function for response and correlation functions derived from \code{\link{dcc}}.
}
\usage{
dcplot(x, ci = TRUE, sig = TRUE, labels = NULL, vertical = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{data.frame} with coefficients derived from \code{\link{dcc}}.
}
  \item{ci}{
logical: should confidence intervals be plotted?
}
  \item{sig}{
logical: should significant coefficients be indicated by bars in darker grey?
}
  \item{labels}{
character vector with labels to use for coefficients. Defaults to \code{rownames(x)}. 
}
  \item{vertical}{
logical: should plots be laid out vertically (defaults to FALSE).
}
}
\details{
An arbitrary number of parameters can be displayed either horizontally
or vertically in subplots.
}
\value{
None. Invoked for side effect (plot).
}
\author{
Christian Zang
}

\seealso{
\code{\link{dcc}}
}
\examples{
\donttest{
data(muc.clim)
data(muc.spruce)

# calculate and plot bootstrapped correlation function
dc <- dcc(muc.spruce, muc.clim, method = "corr")
dcplot(dc)
}
}

\keyword{ hplot }
