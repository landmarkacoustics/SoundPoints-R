\name{spectrogram}
\alias{spectrogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  make a spectrogram from a file in WAV format
}
\description{
  computes a spectrogram with Hamming windows and user-defined raster resolution
}
\usage{
spectrogram(path, time.res = 1/1000, freq.res = 100)
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
  an object of class "spg", which is a matrix with the following
  additional attributes:
  \item{time}{ a vector of time values for each row }
  \item{frequency}{ a vector of frequency mid-points for each column }
  \item{RMS}{ the root-mean-square amplitude of each row }
  \item{envelope}{ a matrix of highest and lowest amplitudes for each
    time step. useful for plotting waveforms}
}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...

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
  \code{\link{hamming}}
  \code{\link{get.freq.res}}
  \code{\link{get.time.res}}
  \code{\link{sample.rate.spg}}
  \code{\link{standard.spg.image}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (path, time.res = 1/1000, freq.res = 100) 
{
    require(tuneR)
    wav <- readWave(path)
    sample.rate <- attr(wav, "samp.rate")
    W <- 2^ceiling(log(sample.rate, 2) - log(freq.res, 2) - 1)
    span <- -(W - 1):W
    S <- floor(time.res * sample.rate)
    wav <- as.numeric(attr(wav, "left"))
    t.max <- length(wav)
    N <- 1 + floor(t.max/S)
    spg <- matrix(NA, N, W)
    envelope <- matrix(NA, N, 2)
    rms <- numeric(N)
    H <- hamming(2 * W)
    for (i in 1:N) {
        ix <- S * (i - 1) + span
        ix[ix < 1 | ix > t.max] <- NA
        envelope[i, ] <- range(wav[ix], na.rm = T)
        win <- H * wav[ix]
        win[is.na(win)] <- 0
        rms[i] <- sqrt(mean(win^2))
        spg[i, ] <- log(spk(win), 10)
    }
    attributes(spg) <- c(attributes(spg), list(time = 0:(N - 
        1) * S/sample.rate, frequency = sample.rate * 0:(W - 
        1)/(2 * W), RMS = rms, envelope = envelope))
    class(spg) <- c("spg", class(spg))
    invisible(spg)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
