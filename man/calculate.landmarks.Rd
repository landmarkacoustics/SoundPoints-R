\name{calculate.landmarks}
\alias{calculate.landmarks}

\title{
  aggregates a peak into landmark descriptions of events
}
\description{
  given a peak contour, event ids, and landmark ids, aggregates the
  contour into a data frame that contains landmarks sets for each event
}
\usage{
calculate.landmarks(peaks, evs.and.lmrks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{peaks}{
    a data frame containing a description of peak contours
  }
  \item{evs.and.lmrks}{
    a data frame containing an event factor and a landmark factor
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  a data frame with a bunch of shiny landmark measurements
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
  \code{\link{assign.landmarks}}
  \code{\link{detect.events}}
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (peaks, evs.and.lmrks) 
{
    fa <- is.finite(evs.and.lmrks$event)
    landmarks <- aggregate(peaks$time[fa], evs.and.lmrks[fa, 
        ], min)
    names(landmarks)[3] <- "start.time"
    landmarks <- cbind(landmarks, aggregate(peaks[fa, 1:8], evs.and.lmrks[fa, 
        ], mean)[, -(1:2)])
    landmarks$time <- landmarks$time - landmarks$start.time
    landmarks <- landmarks[order(landmarks$event, landmarks$landmark), 
        ]
    return(invisible(landmarks))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ morphometrics }
\keyword{ sound }% __ONLY ONE__ keyword per line
