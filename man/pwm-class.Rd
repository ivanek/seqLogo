\name{pwm-class}
\docType{class}
\alias{pwm-class}
\alias{summary,pwm-method}
\alias{print,pwm-method}
\alias{show,pwm-method}
\alias{plot,pwm-method}

\title{Class "pwm"}
\description{
  An object of class \code{"pwm"} represents the 4xW position weight
  matrix of a DNA sequence motif. The entry in row i, column j gives the
  probability of observing nucleotide \code{c("A","C","G","T")[i]} in
  position j of the motif.  
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("pwm", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{pwm}:}{Object of class \code{"matrix"} The position
      weight matrix.}
    \item{\code{width}:}{\code{"numeric"} The width of the motif.}
    \item{\code{alphabet}:}{\code{"character"} The sequence
      alphabet. Currently, only "DNA" is supported.}
  }
}
\section{Methods}{
 \describe{
  \item{summary}{\code{signature(object = "pwm",...)} Prints the
    position weight matrix. }

  \item{print}{\code{signature(x = "pwm",...)} Prints the
    position weight matrix.}

  \item{show}{\code{signature(object = "pwm")} Prints the
    position weight matrix. }

  \item{plot}{\code{signature(x = "pwm")} Plots the
    sequence logo of the position weight matrix. }
 } 
}
\references{}
\author{Oliver Bembom, \email{bembom@berkeley.edu}}

\note{}


\seealso{
}
\examples{
}
\keyword{classes}
