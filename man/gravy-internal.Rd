\name{gravy-internal}
\alias{coef.HOF}
\alias{deviance.HOF}
\alias{df.residual.HOF}
\alias{GaussPara.optop.HOF}
\alias{GaussPara.tol.HOF}
\alias{gradder.HOF}
\alias{HOF1}
\alias{pick.model}
\alias{scale01}  
\alias{ssHOF}
\alias{logLik.HOF}

\title{Internal gravy functions}

\description{
  Internal gravy functions.
}
\usage{
\method{coef}{HOF}(object, model, ...)
\method{deviance}{HOF}(object, model, ...)
\method{logLik}{HOF}(object, ...)
\method{df.residual}{HOF}(object, model, ...)
pick.model(obj, level = 0.95,  test = c("F", "Chisq", "AIC", "BIC"), ...)
scale01(x, xrange)
ssHOF(x,y,M,model=5)
}

\details{
  These are not to be called by the user.
}

\keyword{internal }

