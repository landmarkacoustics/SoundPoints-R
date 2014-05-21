\name{get.freq.res}
\alias{get.freq.res}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract the frequency resolution from a spectrogram
}
\description{
  computes the difference between frequency bins in the spectrogram.
}
\usage{
get.freq.res(spg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spg}{
    a spectrogram object
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  a numeric value
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
  \code{\link{spectrogram}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (spg) 
{
    return(diff(attr(spg, "frequency")[1:2]))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
