\name{plotGrad}
\alias{plotGrad}

\title{ Plots Species gainst an Environmental Gradient }
\description{
  Plots each species in a data frame against an environmental
  gradient using trellis graphics.
}
\usage{
plotGrad(x, grad, freq.limit = 0, ...)
}

\arguments{
  \item{x}{ Data frame with species data. }
  \item{grad}{ Gradient vector. }
  \item{freq.limit}{ Lowest frequency for species selected for plotting }
  \item{\dots}{ Other parameters to \code{\link[lattice]{Lattice}} functions. }
}
\details{
  Function \code{plotGrad} is intended for preliminary inspection of
  the data. It plots each species in a data frame against an
  environmental gradient using \code{\link[lattice]{Lattice}}
  functions. If \code{grad} is numeric, it uses scatter plots
  (\code{\link[lattice]{xyplot}}) and if \code{grad} is a factor, it
  uses boxplots (\code{\link[lattice]{bwplot}}).

  All species are plotted with equal vertical scale, making scarce
  species very flat. Users can change this behaviour by transforming or
  standardizing species data before analysis (for instance,
  \code{\link[vegan]{decostand}}) has options \code{"max"} for making all
    species to equal scale).

  The function uses trellis graphics in package \code{lattice}, and
  transfers all parameters to underlying functions
  \code{\link[lattice]{xyplot}} and
  \code{\link[lattice]{bwplot}}. Moreover, many graphical parameters can
  be changed after producing the graph (see
  \code{\link[lattice]{trellis.par.set}}).  
}
\value{
  Function returns a \code{"trellis"} object.
}

\author{
  Jari Oksanen
}

\seealso{
  See \code{\link[lattice]{Lattice}} basics of Trellis graphics, 
  \code{\link[lattice]{xyplot}} and \code{\link[lattice]{bwplot}} for
  underlying plotting functions, and \code{\link[lattice]{trellis.par.set}} for
  setting graphical parameters.
}

\examples{
data(mtf01)
data(mtf.alt)
attach(mtf.alt)
fig  <- plotGrad(mtf01, Altitude)
fig
trellis.par.set(theme = col.whitebg())
fig
}
\keyword{ dplot }

