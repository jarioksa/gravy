\name{hillscale}
\alias{hillscale}
\alias{betahill}
\alias{plot.hillscale}

\title{Hill indices of Beta diversity and gradient scaling to constant
  Hill index 2 }
\description{
  The function finds two kinds of Hill indices of Beta diversity and
  tries to scale the gradient to constant Hill index 2 by segments.
}
\usage{
hillscale(veg, grad, cycles = 4, freq.lim = 1)
betahill(veg, grad, freq.lim = 1)
\method{plot}{hillscale}(x, which=c(1,2), xlab, ...)
}

\arguments{
  \item{veg}{Community data matrix. }
  \item{grad}{Environmental gradient. }
  \item{cycles}{Number of Hill scaling cycles in rescaling. }
  \item{freq.lim}{Frequency limit for including species.}
  \item{which}{Plots for indices 1 and 2.}
  \item{xlab}{Label for graphs. If missing, gradient name used.}
  \item{x}{A \code{"hillscale"} result object.}
  \item{...}{Other graphical parameters.}
}
\details{
  Mark Hill (1979) suggested two indices of Beta diversity:
  \enumerate{
    \item Mean width of species responses, measured as weighted standard
    deviation of gradient values.
    \item Weighted variance of species scores within a site.
  }
  Hill & Gauch (1980) discuss only the former index, but the program
  \code{\link[vegan]{decorana}} uses only the second index.
  Function \code{betahill} calculates both indices for all sample plots. In
  addition, the function smooths these on 20 segments along the
  gradient, using the same algorithm as \code{\link[vegan]{decorana}}
  (Hill 1979, Hill & Gauch 1980).

  Function \code{hillscale} rescales the gradient by segments using Hill
  index 2 (weighted variance of species scores) following as faithfully
  as possible the rescaling algorithm in \code{\link[vegan]{decorana}}.
  However, the function evaluates Hill index 1 (mean weighted sd of
  species), unlike \code{\link[vegan]{decorana}}.

  The major difference is that \code{\link[vegan]{decorana}} scales a
  correspondence analysis axis where site scores are direct weighted
  averages of species scores. Function \code{hillscale} uses original
  gradient values, but finds the species scores as weighted averages of
  gradient values, and expands the species scores that they have the
  same weighted variance as the species scores would have in
  correspondence analysis. If a correspondence analysis axis is given as
  a gradient, same species scores will be found as in
  \code{\link[vegan]{decorana}}. Another major difference is that
  \code{\link[vegan]{decorana}} never rescales site scores. It rescales
  species scores instead, and always finds the site scores as direct
  weighted averages of species scores. Function \code{hillscale}
  rescales gradients. This difference is so significant that a rescaled
  correspondence analysis axis will be different in \code{hillscale} and
  \code{\link[vegan]{decorana}}. 
}
\value{
  Both functions return an object of class \code{"hillscale"} with
  following items:

  \item{grad }{Rescaled gradient in \code{hillscale} or the original
    gradient in \code{betahill}}
  \item{Hill.1 }{Hill index 1 (mean weighted sd).}
  \item{Hill.2}{Hill index 2 (weihted variance of species scores).}
  \item{zv1}{The smoothed values of Hill.1 on 20 segments.}
  \item{zv2}{The smoothed values of Hill.2 on 20 segments.}
  \item{rug}{21 rug tics equally distributed on the original gradient.}
  \item{cycles}{The number of rescaling cycles.}
  \item{gradname}{The name of the gradient variable.}
  \item{Call}{The function call.}
}
\references{
  Hill, M.O. (1979) \emph{DECORANA: a FORTRAN program for detrended
    correspondence analysis and reciprocal averaging}. Cornell
  University, Ithaca, NY.

  Hill, M.O. & Gauch, J.G. (1980) Detrended correspondence analysis: an
  improved ordination technique. \emph{Vegetatio} 42, 47-58.
}

\author{ Jari Oksanen}

\seealso{\code{\link{betadiversity}}, \code{\link{gradscale}},
  \code{\link[vegan]{decorana}}.  }

\examples{
## None yet (no suitable data in the package)
}
\keyword{ models }

