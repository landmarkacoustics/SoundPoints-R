\name{plot.spg}
\alias{plot.spg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  plots a spectrogram image in time-frequency space
}
\description{
  this makes a large bitmap with one rectangle for each cell of the
  spectrogram. the colors represent amplitude.
}
\usage{
plot.spg(x, pixel.colors = gray.colors(16, 1, 0), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    a spectrogram object
}
  \item{pixel.colors}{
a vector of colors for different amplitudes
}
  \item{\dots}{
    other arguments for image
}
}
\details{
the image function makes VERY large PDFs because each pixel is stored as
a rectangle. if you are going to output with this function for
publication, use a bitmap format first and mash them up in a graphics editor.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
  \code{\link{image}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, pixel.colors = gray.colors(16, 1, 0), ...) 
{
    image(attr(x, "time"), attr(x, "frequency"), x, col = pixel.colors, 
        xlab = "Time (s)", ylab = "Frequency (Hz)", ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
