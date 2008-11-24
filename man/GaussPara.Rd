\name{GaussPara}
\alias{GaussPara}

\title{Finds Gaussian Parameters of a Response Model.}
\description{
  \code{GaussPara} is a generci function to find Gaussian parameters in
  any response function. In Gaussian function, these will be the real
  Gaussian paramters, in other models similar parameters for the height
  at optimum (\code{"top"}), location of the optimum (\code{"opt"}) and
  the width of the response at height \code{exp(-0.5)*top}. For
  unsymmetric responses, parameter \code{tol} gives the width to the
  left and \code{tol.right} the width to the right of \code{opt}.
}
\usage{
GaussPara(resp, ...)
}

\arguments{
  \item{resp}{Response function for a single species or a list of
    responses from a data frame method. }
  \item{\dots}{Other parameters. See individual response models for
    possible other paraemters. }
}
\details{
  See individual response functions for more details.
}
\value{
  Returns an object with (at least) items \code{top} for the height at the
  optimum, \code{opt} for the location of the optimum), \code{tol} for
  the width to the left of the optimum, \code{tol.right} width to the right
  of the optimum or text \code{symmetric} for symmetric
  responses. Individual response functions may add some items to these.
}
\references{
  Lawesson, J.E. & Oksanen, J. (2002) Niche characteristics of Danish
  woody species as derived from coenoclines. \emph{Journal of Vegetation
    Science} 13: 279-290. (For HOF models.)
}
\author{ Jari Oksanen }
\note{
  Currently implemented only for \code{\link{HOF}}.
}


\seealso{
  \code{\link{HOF}} is the only implementation. If the parameters cannot
  be found in closed form, \code{\link{optimize}} is used for \code{opt}
  and \code{\link{uniroot}} for \code{tol} and \code{tol.right}.
}

\examples{
data(mtf01)
data(mtf.alt)
attach(mtf.alt)
mods <- HOF(mtf01, Altitude, 1)
GaussPara(mods)
}
\keyword{ models }
\keyword{ regression }
