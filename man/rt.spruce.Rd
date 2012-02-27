\name{rt.spruce}
\docType{data}
\alias{rt.spruce}
\title{Tree-Ring Chronology of a Spruce Population at Rothenburg ob der Tauber}
\description{
  This dataset gives the tree-ring indices for \emph{Picea abies} at
  Rothenburg ob der Tauber, Bavaria, Germany. The chronology represents
  20 cores from 10 trees. The series were read in using
  \code{\link[dplR]{read.rwl}} from package \code{dplR}, detrended using a 60a
  spline with 50\% frequency cutoff (function \code{\link[dplR]{detrend}}),
  and averaged to a chronology using a robust mean \code{\link[dplR]{chron}}.
}
\usage{rt.spruce}
\format{A \code{data.frame} containing tree-ring indices and replication
  depth with respective years as \code{rownames}.}
\references{
  Zang, C., Rothe, A., Weis, W., Pretzsch, H., 2011. Zur Baumarteneignung bei Klimawandel:
Ableitung der Trockenstress-Anf\"{a}lligkeit wichtiger Waldbaumarten aus
Jahrringbreiten. Allgemeine Forst- und Jagdzeitung 182, 98-112.
}
\keyword{datasets}