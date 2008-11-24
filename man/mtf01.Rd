\name{mtf01}
\alias{mtf01}
\alias{mtf.alt}
\non_function{}
\title{ Species Data and Altitude from Mt. Field, Tasmania. }
\usage{
data(mtf01)
data(mtf.alt)
}

\description{
The \code{mtf01} data frame has 167 sites (rows)  and 5 species
(columns). The data are a subset of well drained sites from a more
extensive data set. Data frame \code{mtf.alt} has only one variable:
\code{Altitude} above sea level (in meters) for each site.
}
\format{
  The species data frame contains the following species:
  \describe{
    \item{EPACSERP}{a numeric vector}
    \item{CYATPETI}{a numeric vector}
    \item{NOTHCUNN}{a numeric vector}
    \item{POA.GUNN}{a numeric vector}
    \item{BAUERUBI}{a numeric vector}
  }
}

\references{
  Minchin, P.R. (1989). Montane vegetation of the Mt. Field 
  massif, Tasmania: a test of some hypotheses about properties of
  community patterns. \emph{Vegetatio 83}, 97.110.  }

\examples{
data(mtf01)
data(mtf.alt)
}

\keyword{datasets}

