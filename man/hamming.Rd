\name{hamming}
\alias{hamming}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An unnormalized Hamming window function of specified size
}
\description{
Uses the formula from Wikipedia to calculate a Hamming window,
symmetrical around its middle element
}
\usage{
hamming(N)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
the length of the output vector
}
}
\details{
  \code{hamming(N) = 0.53836 - 0.46164 * cos(2 * pi * 0:(N - 1)/(N - 1))}
}
\value{
  a vector of length N that holds the weights of a hamming window.
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (N) 
{
    0.53836 - 0.46164 * cos(2 * pi * 0:(N - 1)/(N - 1))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
