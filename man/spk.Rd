\name{spk}
\alias{spk}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
compute a frequency spectrum
}
\description{
this is an internal function for shorthand purposes
}
\usage{
spk(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a real-valued input vector
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a vector of spectral coefficients corresponding to a real-to-real FFT
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
\code{\link{fft}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
    n <- floor(length(x)/2)
    s <- fft(x)
    Re(Conj(s) * s)[1:n]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
