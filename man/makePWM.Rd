\name{makePWM}
\alias{makePWM}
\title{ Constructing a pwm object}
\description{
  This function constructs an object of class pwm from a matrix. It
  checks that the matrix has correct dimensions and that columns add up
  to 1.0.
  }
\usage{
makePWM(pwm, alphabet="DNA")
}
\arguments{
  \item{pwm}{\code{matrix} representing the positon weight matrix}
  \item{alphabet}{\code{character} the alphabet making up the
    sequence. Currently, only "DNA" is supported. }
}
\value{
  An object of class pwm.
}
\author{
Oliver Bembom, \email{bembom@berkeley.edu}
}
\examples{
mFile <- system.file("Exfiles/pwm1", package="seqLogo")
m <- read.table(mFile)
pwm <- makePWM(m)
}
\keyword{misc}
