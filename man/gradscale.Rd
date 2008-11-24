\name{gradscale}
\alias{gradscale}
\title{ Rescales an ecological gradient to a constant rate of change of
  community composition.}
\description{
  Function \code{gradscale} tries to scale an ecological gradient to a
  constant rate of compositional change. Usually this is not strictly
  possible. 
}
\usage{
gradscale(resp, grad, ...)
}

\arguments{
  \item{resp}{ A data frame of fitted species response models. }
  \item{grad}{ The gradient used for responses \code{resp}.  }
  \item{\dots}{ Other parameters. }
}
\details{
  Function \code{gradscale} tries to rescale an ecological gradient to
  the constant rate of compositional change as measured by
  \code{\link{betadiversity}}. Usually this is not strictly possible,
  because the responses fitted after rescaling indicate local
  aberrations, although the average change is close to constant.

  The function \code{gradscale} makes the distance of ordered gradient
  points equal to sum of change in species abundance.  In most cases,
  this is equal to change in fitted values, but if the species has its
  optimum between the gradient values, \code{gradscale} will estimate
  the accumulated change through that optimum point.
}
\value{
  The function returns the rescaled gradient at the point given by
  \code{grad}. 
}
\references{
  Oksanen, J. & Tonteri, T. (1995). Rate of compositional turnover along
  gradients and total gradient length. \emph{Journal of Vegetation
    Science} 6, 815-824. 
}
\author{Jari Oksanen. }
\seealso{\code{\link{betadiversity}}. }

\examples{
data(mtf01)
data(mtf.alt)
attach(mtf.alt)
mod <- HOF(mtf01, Altitude, M=1)
beta <- betadiversity(mod, Altitude)
plot(beta)
Alt.s <- gradscale(mod, Altitude, M=1)
mod.s <- HOF(mtf01, Alt.s, M=1)
plot(betadiversity(mod.s, Alt.s))
}
\keyword{models }

