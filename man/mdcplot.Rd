\name{mdcplot}
\Rdversion{1.1}
\alias{mdcplot}
\title{
Plotting Function for Moving Response and Correlation Functions
}
\description{
A simple plotting function for response and correlation functions derived from \code{\link{mdcc}}.
}
\usage{
mdcplot(x, rescale = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{data.frame} with coefficients derived from \code{\link{mdcc}}.
}
  \item{rescale}{
logical: should coefficients be rescaled to use full color gradient?
}
\item{...}{
additional arguments passed to plot(...)
}
}
\details{
Rescaling of coefficients results in more contrast for color
palette. For comparison of absolute values (between different plots),
this should be set to FALSE. Rescaling is done seperately for positive
and negative values.

Previous to bootRes version 1.2.2, \code{\link{mdcc}} would return a
single \code{data.frame} of coefficients, regardless if bootstrapping
was enabled or not. In more recent versions, a \code{list} of
\code{data.frames} is returned, containing members for coefficients and
confidence intervals. The type of \code{x} is checked internally, so
there is no change in the interface of \code{mdcplot}.
}
\value{
None. Invoked for side effect (plot).
}
\author{
Christian Zang
}

\seealso{
\code{\link{mdcc}}
}
\examples{
data(muc.clim)
data(muc.spruce)

# calculate and plot bootstrapped correlation function
mdc <- mdcc(muc.spruce, muc.clim, method = "corr")
mdcplot(mdc)
}

\keyword{ hplot }