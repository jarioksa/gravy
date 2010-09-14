\name{boxgradient}
\alias{boxgradient}
\alias{gaussgradient}

\title{Display Species Response along Gradients as Boxplots or Gaussian Models }
\description{
  The function display species responses along ecological gradients
  using either \code{\link{boxplot}} or a \code{\link{polygon}}
  displaying fitted Gaussian responses.
}
\usage{
boxgradient(x, grad, horizontal = TRUE, xlab, freq.lim = 5,
    cex.species= 0.7, axes = TRUE, ...)
gaussgradient(x, grad, family = poisson, xlab, freq.lim = 5,
    cex.species = 0.7, axes = TRUE, ...)
}
\arguments{
  \item{x}{Community data }
  \item{grad}{Gradient vector }
  \item{horizontal}{Use horizontal \code{\link{boxplot}}. }
  \item{family}{Errof \code{\link{family}} used in fitting the Gaussian
    model using \code{\link{glm}}. The \code{link} function will always
    be \code{log}, and some families cannot be fitted with this link.}
  \item{xlab}{Label of the gradient.  Variable name used as default.}
  \item{freq.lim}{Frequency of rarest species displayed.}
  \item{cex.species}{Size multiplier for species labels.  Species labela
  are printed horizontally, and typically you must reduces their size
  with this parameter or increase the margin (see \code{mar} in
  \code{\link{par}}), or \code{\link{abbreviate}} names.}
  \item{axes}{Draw \code{axis} with labels. }
  \item{\dots}{Other parameters passed to underlying functions
    \code{\link{boxplot}} or \code{\link{polygon}}. }
}
\details{
  The functions are intended for simultaneous display of species
  responses along an ecological gradient.

  Function \code{boxgradient}
  will draw \code{\link{boxplot}}s of species presences.  In addition,
  it puts points at weighted avarages. The boxplots are based on
  presence data only, but weighted averages are based on the original
  quantitative information.  Boxes are arranged by medians, but breaking
  ties by weighted averages.  The
  function also adds lines of quartiles and median for the whole
  gradient, and adds a \code{\link{rug}} of gradient values.

  Function \code{gaussgradient} draws fitted Gaussian response models.
  The curves are adjusted to the same area so that narrower responses
  will look higher, but the heights have no relation to the original
  abundances.  The responses are fitted using \code{\link{glm}}, and
  species with failed fitting (\dQuote{inverted} responses) will not be
  displayed.  The fitted responses are ordered by the location of the
  top (\sQuote{optimum}). 
}
\value{
  The functions are used to draw plots.  Function \code{boxpgradient} returns
  invisibly the object returned by \code{\link{boxplot}}. Function
  \code{gaussgradient} returns invisibly a \code{list} of fitted optima
  and tolerances.
}

\author{ Jari Oksanen }
\seealso{\code{\link{boxplot}}, \code{\link{polygon}},
  \code{\link{axis}}, \code{\link{par}}. }
\examples{
data(mtf01)
data(mtf.alt)
attach(mtf.alt)
op <- par(no.readonly=TRUE)
par(mfrow=c(2,1))
par(mar=c(4,6,1,1))
boxgradient(mtf01, Altitude, col="pink", border="blue", notch=TRUE)
gaussgradient(mtf01, Altitude, col="pink", border="blue")
par(op)
}
\keyword{ dplot }
\keyword{ models }
