\name{plot.cepstrum}
\alias{plot.cepstrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  plots a cepstrum image in time - quefrency space
}
\description{
i'm still working with how to scale the amplitude values so that the
image is meaningful.
}
\usage{
plot.cepstrum(x, pixel.colors = gray.colors(16, 1, 0), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a cepstrum object
}
  \item{pixel.colors}{
a vector of color levels for the bitmap
}
  \item{\dots}{
    other arguments for image
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
  \code{\link{cepstrum}}
  \code{\link{image}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, pixel.colors = gray.colors(16, 1, 0), ...) 
{
    if (is(x) == "cepstrum") {
        image(attr(x, "time"), attr(x, "quefrency"), x, col = pixel.colors, 
            xlab = "Time (s)", ylab = "Quefrency (Hz)", ...)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
