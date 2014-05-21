\name{mt.spectrogram}
\alias{mt.spectrogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  this attempts a multitaper spectrogram, but I don't think it works...
}
\description{
  i'm trying to be like Ofer here.
}
\usage{
mt.spectrogram(path, time.res = 1/1000, freq.res = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
    the location of the sound file
  }
  \item{time.res}{
    the desired time resolution of the spectrogram
  }
  \item{freq.res}{
    the desired frequency resolution of the spectrogram
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  a spectrogram object
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
function (path, time.res = 1/1000, freq.res = 100) 
{
    require(sound)
    require(multitaper)
    wav <- loadSample(path)
    sample.rate <- wav$rate
    W <- 2^ceiling(log(sample.rate, 2) - log(freq.res, 2) - 1)
    span <- 1:W
    S <- floor(time.res * sample.rate)
    wav <- as.numeric(wav$sound)
    t.max <- length(wav)
    N <- 1 + floor(t.max/S)
    spg <- matrix(NA, N, W + 1)
    envelope <- matrix(NA, N, 2)
    rms <- numeric(N)
    DPSS <- dpss(W, k = 4, nw = 4)
    for (i in 1:N) {
        ix <- S * (i - 1) + span
        ix[ix < 1 | ix > t.max] <- NA
        envelope[i, ] <- range(wav[ix], na.rm = T)
        win <- wav[ix]
        win[is.na(win)] <- 0
        win <- ts(win, start = ix[1]/sample.rate, deltat = 1/sample.rate)
        rms[i] <- sqrt(mean(win^2))
        tmp <- spec.mtm(win, plot = F, k = 4, nw = 4, dpssIN = DPSS, 
            log = "n")
        spg[i, ] <- tmp$spec
    }
    attributes(spg) <- c(attributes(spg), list(time = 0:(N - 
        1) * S/sample.rate, frequency = tmp$freq, RMS = rms, 
        envelope = envelope))
    class(spg) <- c("spg", class(spg))
    invisible(spg)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
