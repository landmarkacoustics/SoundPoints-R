\name{sample.rate.spg}
\alias{sample.rate.spg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  extracts the sample rate from a spectrogram
}
\description{
  'nuff said.
}
\usage{
sample.rate.spg(spg)
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
a single numeric value, the samples per second of the spectrogram's
source file
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
    ifelse(is(spg) == "spg", diff(attr(spg, "frequency")[1:2]) * 
        ncol(spg), NaN)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ spectrogram }

