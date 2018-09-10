\name{collect.absolute.energy}
\alias{collect.absolute.energy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Find the region around the peak value that yields a specificied area under the curve.
}
\description{
  finds the max value of a spectrum, then spreads out from that peak until
  it has determined a region whose area meets a fixed sum. Given the same
  sum, a low peak will yield a broader region than a high one.
}
\usage{
collect.absolute.energy(spk, desired.sum)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spk}{
    a vector that corresponds to a peak trace.
  }
  \item{desired.sum}{
    the area under the curve that you are looking for
  }
}
\details{
  this function assumes all of spk >= 0, and will perform oddly if any
  values are negative.
}
\value{
  a vector with three entries:
  \item{lo}{ the index of the beginning of the region (inclusive) }
  \item{hi}{ the index of the end of the region (inclusive) }
  \item{total}{ the sum of the heights within the region }
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
  \code{\link{spectrum}}
  \code{\link{collect.energy}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (spk, desired.sum) 
{
    total <- max(spk)
    N <- length(spk)
    lo <- hi <- match(total, spk)[1]
    while (total < desired.sum) {
        action.case <- switch(1 + 2 * (is.na(lo) || lo < 2 || 
            spk[lo - 1] < 0) + (is.na(hi) || is.na(spk[hi + 1]) || 
            spk[hi + 1] < 0), ifelse(spk[lo - 1] < spk[hi + 1], 
            1, 2), 1, 2, 3)
        if (action.case == 1) {
            lo <- lo - 1
            total <- total + spk[lo]
        }
        else if (action.case == 2) {
            hi <- hi + 1
            total <- total + spk[hi]
        }
        else {
            break
        }
    }
    if (is.na(lo) || lo < 1) 
        lo <- 1
    if (is.na(hi) || hi > N) 
        hi <- N
    return(c(lo, hi, total))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
