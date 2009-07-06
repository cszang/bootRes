\name{dcplot}
\Rdversion{1.1}
\alias{dcplot}
\title{
Plotting Function for Response and Correlation Functions
}
\description{
A simple plotting function for response and correlation functions derived from \code{\link{dendroclim}}.
}
\usage{
dcplot(x, ci = TRUE, sig = TRUE, labels = NULL, labstyle = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{data.frame} with coefficients derived from \code{\link{dendroclim}}.
}
  \item{ci}{
logical: should confidence intervals be plotted?
}
  \item{sig}{
logical: should significant coefficients be indicated by filled symbols?
}
  \item{labels}{
character vector with labels to use for coefficients. Defaults to \code{rownames(x)}. 
}
  \item{labstyle}{
integer indicating the style to be used for labels. Options are 1 for verbose/verbatim labels or 2 for abbreviated month names. If custom labels are specified, selection reverts to 1.
}
}
\value{
None. Invoked for side effect (plot).
}
\author{
Christian Zang
}

\seealso{
\code{\link{dendroclim}}
}
\examples{
data(muc.clim)
data(muc.spruce)

# calculate and plot bootstrapped correlation function
dc <- dendroclim(muc.spruce, muc.clim, method = "corr")
dcplot(dc, labstyle = 2)
}

\keyword{ hplot }