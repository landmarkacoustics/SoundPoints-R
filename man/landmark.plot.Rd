\name{landmark.plot}
\alias{landmark.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Add the points from a landmark matrix to an existing plot
}
\description{
Adds the landmarks from each event in a landmark matrix to an existing
plot. The points cycle through red, green, and blue so that each event
is distinct from its neighbors.
}
\usage{
landmark.plot(landmarks, ix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{landmarks}{
a matrix of landmarks
}
  \item{ix}{
    indices of columns that all correspond to the same variable
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  none
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Benjamin N. Taft \email{ben.taft@landmarkacoustics.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{soundpoints}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (landmarks, ix) 
{
    points(t(landmarks$mean.time + landmarks[, 11 + ix]), t(landmarks[, 
        15 + ix]), pch = 16, cex = 2/3, col = rep(2:4, each = length(ix)))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ morphometrics }% __ONLY ONE__ keyword per line
