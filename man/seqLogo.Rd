\name{seqLogo}
\alias{seqLogo}
\title{Plot a sequence logo for a given position weight matrix}
\description{
  This function takes the 4xW position weight matrix of a DNA sequence
  motif and plots the corresponding sequence logo. 
}
\usage{
  seqLogo(pwm, ic.scale=TRUE, xaxis=TRUE, yaxis=TRUE, xfontsize=15, yfontsize=15)
}
\arguments{
  \item{pwm}{\code{numeric} The 4xW position weight matrix.}
  \item{ic.scale}{\code{logical} If TRUE, the height of each column is
    proportional to its information content. Otherwise, all columns have
    the same height.}
  \item{xaxis}{\code{logical} If TRUE, an X-axis will be plotted.}
  \item{yaxis}{\code{logical} If TRUE, a Y-axis will be plotted.}
  \item{xfontsize}{\code{numeric} Font size to be used for the X-axis.}
  \item{yfontsize}{\code{numeric} Font size to be used for the Y-axis.} 
}
\details{
  Within each column, the height
  of a given letter is proportional to its frequency at that position.
  If ic.scale is TRUE, the height of each column in the plot indicates
  the information
  content at that position of the motif. Otherwise, the height of all
  columns are identical.
}
\value{
  None.
}
\author{Oliver Bembom, \email{bembom@berkeley.edu}}

\examples{
mFile <- system.file("Exfiles/pwm1", package="seqLogo")
m <- read.table(mFile)
pwm <- makePWM(m)
seqLogo(pwm)
}
\keyword{misc}
