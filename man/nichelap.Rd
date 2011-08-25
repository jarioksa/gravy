\name{nichelap}
\alias{nichelap}
\alias{nichelap.HOF}
\alias{nichelap.HOF.frame}
\alias{as.matrix.nichelap.HOF.frame}

\title{Overlap of Two Species Responses along a Gradient. }
\description{
  Functions estimates the amount of overlap of two fitted species
  responses along a single gradient. 
}
\usage{
\method{nichelap}{HOF}(sp1, sp2, test = "BIC", ...)
\method{nichelap}{HOF.frame}(df, test = "BIC", ...)
\method{as.matrix}{nichelap.HOF.frame}(x, ...)
}

\arguments{
  \item{sp1, sp2}{Fitted response models for two species.}
  \item{df}{Fitted responses for a data frame of several species.}
  \item{test}{The test used to select the HOF model.}
  \item{x}{The result of nichelap for a data frame of several species.}
  \item{\dots}{Other arguments passed to functions.}
}
\details{
  The function finds the niche overlap as an overlap of fitted response
  curves (Lawesson & Oksanen 2002). The input can either consist of
  fitted responses for two species, or of responses fitted to several
  species in a data frame. If the input is a frame of several fitted
  responses, overlaps will be found for each pair of species.

  For each pair of species, the function will return a vector with total
  areas for each species, the total area of the overlap, the proportion
  of overlap from the total covered jointly by two species, and the
  proportion of each species covered by the other species. The total
  area is defined so that the the maximum response height and total
  gradient range is unity. Consequently, the reported areas are
  proportions of the maximum attainable area (species occurs at its
  attainable maximum over the whole gradient range). Function
  \code{as.matrix} returns the last two entries in a matrix similar to
  Lawesson & Oksanen (2002), table 4.

  The functions are based on the standard \R\ function
  \code{\link{integrate}}. 
}
\value{
  Function returns a vector of niche statistic for each pair of
  species. For a response frame, these vectors are each an item in a
  list. 
}
\references{
  Lawesson, J.E. & Oksanen, J. (2002). Niche characteristics of Danish
  woody species as derived from coenoclines. \emph{Journal of Vegetation
    Science} 13, 279--290.
}
\author{Jari Oksanen }
\note{
  At the moment, the function knows only HOF models.
}

\seealso{\code{\link{integrate}}, \code{\link{HOF}}. }
\examples{
data(mtf01)
data(mtf.alt)
attach(mtf.alt)
mods <- HOF(mtf01, Altitude, M=1)
lap <- nichelap(mods)
lap
mat <- as.matrix(lap)
mat
# Similar formatting as in Lawesson & Oksanen (2002)
round(100*mat)
}
\keyword{models }

