\name{standard.spg.image}
\alias{standard.spg.image}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  make a bitmap picture of a spectrogram that should look nice after
  some tweaking
}
\description{
  makes a black-and-white bitmap with darkness denoting amplitude, with
  lots of user control over appearance. The output file is in PNG format
}
\usage{
standard.spg.image(spg, path = "default.png", amplitude.threshold = 0, time.offset = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spg}{
    a spectrogram object
  }
  \item{path}{
    the location of the image file that will be produced
  }
  \item{amplitude.threshold}{
    any amplitude less than this threshold will be ignored
  }
  \item{time.offset}{
    you may want to shift the time axis to match up with other files
  }
  \item{\dots}{
    further arguments to plot.spg
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
  \code{\link{plot.spg}}
  \code{\link{png}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (spg, path = "default.png", amplitude.threshold = 0, 
    time.offset = 0, ...) 
{
    if (is(spg) == "spg") {
        png(path, 5 * 180, 3 * 180, pointsize = 10, res = 180)
        par(mar = c(10, 10, 2, 2)/3, mgp = 2 * c(3, 1, 0)/3)
        temp <- -spg
        temp[temp > amplitude.threshold] <- amplitude.threshold
        image(attr(spg, "time"), attr(spg, "frequency")/1000, 
            temp, col = gray.colors(512, 0, 1), xlab = "Time (s)", 
            xlim = c(0, 3) + time.offset, ylab = "Frequency (kHz)", 
            ylim = c(0, 10), ...)
        dev.off()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sound }
\keyword{ image }% __ONLY ONE__ keyword per line
