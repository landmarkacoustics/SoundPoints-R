\name{collect.energy}
\alias{collect.energy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
finds a region the under the highest point in a 1-D landscape
corresponding to a fixed area}
\description{
goes to the max of the input vector, then walks out in either direction
until the user-specified amplitude sum is achieved
}
\usage{
collect.energy(spk, desired.sum)
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
  \code{\link{collect.absolute.energy}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (spk, desired.sum) 
{
    ends <- rep(match(max(spk), spk), 2)
    N <- length(spk)
    total <- 0
    current.step <- 1
    while (total < desired.sum) {
        temp.next <- c(ifelse(ends[1] > 1, spk[ends[1] - 1], 
            -Inf), ifelse(ends[2] < N, spk[ends[2] + 1], -Inf))
        now <- 1 + (temp.next[1] < temp.next[2])
        if (is.infinite(temp.next[now])) 
            break
        height <- spk[ends[now]] - temp.next[now]
        width <- ifelse(height > 0, current.step, 1)
        ends[now] <- ends[now] + ifelse(now == 1, -1, 1)
        total <- total + width * height
        current.step <- current.step + 1
    }
    return(c(ends, total))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ peak finding }
