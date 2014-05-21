\name{smooth.average}
\alias{smooth.average}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  A weighted mean smooth of a time series
}
\description{
the weight of previous elements is determined by the resolution of the
time series and the half-life of the weighting function
}
\usage{
smooth.average(amplitude, time.res, half.life, initial.value = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{amplitude}{
a vector of real-valued numbers
}
  \item{time.res}{
the time interval separating the elements of \code{amplitude}
}
  \item{half.life}{
the time it takes for half of an element's impact to disappear. 
}
  \item{initial.value}{
the starting value of the smooth. it's usually good to start with
something other than \code{amplitude[0]}, like an estimate of \code{mean(amplitude)}
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  a numeric vector the same length as \code{amplitude}
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
function (amplitude, time.res, half.life, initial.value = 1) 
{
    N <- length(amplitude)
    smooth.amplitude <- rep(initial.value, N)
    k <- time.res/half.life
    for (i in 2:N) {
        smooth.amplitude[i] <- (1 - k) * smooth.amplitude[i - 
            1] + k * amplitude[i]
    }
    return(invisible(smooth.amplitude))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ time series }
