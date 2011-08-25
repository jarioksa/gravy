\name{betadiversity}
\alias{betadiversity}
\alias{betadiversity.HOF.frame}
\alias{plot.betadiversity}

\title{Estimates Beta Diversity as the instantaneous rate of change at
  any gradient point. }
\description{
  The function \code{betadiversity} estimates the beta diversity as the
  instantenous rate of change at any gradient point.
}
\usage{
betadiversity(object, x, ...)
\method{plot}{betadiversity}(x, type="b", xlab, ylab, splines = FALSE,  ...)
}
\arguments{
  \item{object}{A response frame object: Fitted models for species. }
  \item{x}{Gradient values.}
  \item{type}{Type of graph.}
  \item{xlab, ylab}{Axis labels; defaults provided if these are
    missing.}
  \item{splines}{Use interpolating \code{\link{spline}} to draw smooth
    lines.} 
  \item{\dots}{Other parameters. }
}
\details{
  The function finds the instantenous rate of change along the gradient
  from the fitted response functions for species.

  Currently the function is implemented for \code{\link{HOF}} models
  only.

  The function has a \code{plot} method.
}
\value{
  The function returns an object of class \code{"betadiversity"} with
  the following items:
  \item{x }{Used gradient values.}
  \item{beta }{Beta diversity at the gradient points.}
  ...
}
\references{
  Oksanen, J. & Tonteri, T. (1995). Rate of compositional turnover along
  gradients and total gradient length. \emph{Journal of Vegetation
    Science} 6, 815-824. 
}
\author{ Jari Oksanen }

\seealso{\code{\link{HOF}}. }

\examples{
data(mtf01)
data(mtf.alt)
attach(mtf.alt)
mod <- HOF(mtf01, Altitude, 1)
x <- seq(min(Altitude), max(Altitude), len=101)
beta <- betadiversity(mod, x)
plot(beta)
}
\keyword{models }

