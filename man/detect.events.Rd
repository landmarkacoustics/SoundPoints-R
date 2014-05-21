\name{detect.events}
\alias{detect.events}
\title{
splits a frequency contour into separate events based upon distances
between adjacent instants.
}
\description{
steps along a frequency contour and calculates the weighted distance
between consecutive instants. if the weighted distance is > 1 then a new
event begins
}
\usage{
detect.events(X, var.weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
    a data frame of frequency contour measurements
  }
  \item{var.weights}{
    the weights of the variables in X
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  a factor of event ids, one for each instant
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
  \code{\link{make.peaks}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (X, var.weights) 
{
    events <- rep(1, nrow(X))
    for (j in 1:ncol(X)) X[, j] <- X[, j] * var.weights[j]
    for (i in 2:nrow(X)) {
        events[i] <- events[i - 1]
        delta <- sum((X[i, ] - X[i - 1, ])^2)
        if (delta > 1) 
            events[i] <- events[i] + 1
    }
    return(invisible(events))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ contour }
\keyword{ event detector }
