\name{HOF}
\alias{HOF}
\alias{HOF.default}
\alias{HOF.data.frame}
\alias{fitted.HOF}
\alias{residuals.HOF}
\alias{plot.HOF}
\alias{plot.HOF.frame}
\alias{predict.HOF}
\alias{GaussPara.HOF}
\alias{GaussPara.HOF.frame}

\title{ Fits Huisman-Olff-Fresco (HOF) Response Models for Species
  along an Ecological Gradient. }
\description{
  Huisman-Olff-Fresco or HOF models are a series of five nested species
  response models which define skewed, symmetric, plateau, monotone and
  flat responses along ecological gradients.
}
\usage{
\method{HOF}{default}(spec, grad, M, y.name, family = binomial, ...)
\method{HOF}{data.frame}(veg, grad, M, freq.limit = 10, ...)
\method{plot}{HOF}(x, ...)
\method{plot}{HOF.frame}(x, level = 0.95, test ="F", species, ...) 
\method{fitted}{HOF}(object, model, ...)
\method{residuals}{HOF}(object, type = c("deviance", "working",
  "response", "pearson"), model, ...)
\method{predict}{HOF}(object, newdata, model, ...)
\method{GaussPara}{HOF}(resp, model, ...)
}

\arguments{
  \item{spec}{Species data vector. }
  \item{veg}{ Vegetation data frame.  }
  \item{grad}{Gradient data vector.}
  \item{M}{ Maximum attainable value in the HOF model, similar to
    binomial denominator. }
  \item{y.name}{Name of the species (stupid, but I used this in
    loops). }
  \item{family}{Error distribution. Alternatives are \code{binomial},
    \code{poisson} and \code{gaussian}.}
  \item{freq.limit}{Lowest frequency of species analysed.}
  \item{level}{Probability for model selection (1-P).}
  \item{test}{Test for model selection. Alternatives are \code{"F"},
    \code{"Chisq"}, \code{"AIC"}, \code{"AICc"} and \code{"BIC"}. }
  \item{type}{the type of residuals which should be returned (see
    \code{\link{residuals.glm}}).}
  \item{species}{Names of the species displayed in graphs.}
  \item{x, object}{An object from \code{HOF(spec, \dots)}.}
  \item{newdata}{Vector of gradient values for prediction.}
  \item{resp}{Fitted response models.}
  \item{model}{Specific HOF model used, if not selected automatically.}
  \item{\dots}{Other parameters }
}

\details{
  Not yet written.

  Function \code{fitted} returns the fitted values for the used
  \code{grad}, and \code{predict} for any values in \code{newdata}. 
}

\value{
  Function \code{HOF.fit} returns an object of class \code{"HOF"} which
  contains the fitting results and other useful information.  }


\references{

  Huisman, J., Olff, H. & Fresco, L.F.M. (1993). A hierarchical
  set of models for species response analysis. \emph{Journal of
    Vegetation Science} 4, 37-46.
  
  Oksanen, J. & Minchin, P.R. (2002). Continum theory revisited: what
  shape are species responses along ecological gradients? \emph{Ecological
  Modelling} 157, 119-129.
 }
\author{ Jari Oksanen }
\note{
  These functions are at their alpha stage: proceed with caution.
}
\seealso{
 \code{\link{plotGrad}}
}

\examples{
data(mtf01)
data(mtf.alt)
attach(mtf01)
attach(mtf.alt)
mod <- HOF(BAUERUBI,Altitude, M=1)
mod
plot(mod)
mod <- HOF(mtf01, Altitude, 1)
plot(mod)
mod
}
\keyword{ models } 
\keyword{ regression }
\keyword{ nonlinear }
