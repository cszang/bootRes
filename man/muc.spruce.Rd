\name{muc.spruce}
\docType{data}
\alias{muc.spruce}
\title{Tree-Ring Chronology of a Spruce Population near Munich}
\description{
  This dataset gives the tree-ring indices for \emph{Picea abies}
  at Forstenrieder Park, Munich, Bavaria, Germany. The chronology represents
  20 cores from 10 trees. The series were read in using
  \code{\link[dplR]{read.rwl}} from package \code{dplR}, detrended using a 60a
  spline with 50\% frequency cutoff (function \code{\link[dplR]{detrend}}),
  and averaged to a chronology using a robust mean \code{\link[dplR]{chron}}.
}
\usage{muc.spruce}
\format{A \code{data.frame} containing tree-ring indices and replication
  depth with respective years as \code{rownames}.}
\references{
  Zang, C., Pretzsch, H., Rothe, A., 2011. Size-dependent responses to
  summer drought in Scots pine, Norway spruce and common oak. Trees -
  Structure and Function.  URL
  http://www.springerlink.com/index/10.1007/s00468-011-0617-z
}
\keyword{datasets}