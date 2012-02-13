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