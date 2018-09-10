\name{make.peaks}
\alias{make.peaks}
\title{
  Extract a continuous peak-frequency contour from a spectrogram
}
\description{
  At each time step of a spectrogram, this function finds the maximum-energy frequency, then identifies the band around that peak that holds a predetermined total amount of energy
}
\usage{
make.peaks(spg, passband, amplitude.cutoff)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spg}{
    a spectrogram object
  }
  \item{passband}{
    a two-element vector that defines the frequency range that will be searched
  }
  \item{amplitude.cutoff}{
    a single numeric value that defines the total area of each peak
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  a data frame in which each row corresponds to a time point of the
  input spectrogram. It has the following columns:
  \item{time}{ the time that the row describes }
  \item{min.frequency}{ the lowest frequency in the peak (inclusive) }
  \item{max.frequency}{ the highest frequency in the peak (inclusive) }
  \item{amplitude.sum}{ the total of all the amplitudes in the peak's range }
  \item{mean.frequency}{ the weighted mean frequency of the peak}
  \item{mean.amplitude}{ the mean amplitude of the peak }
  \item{max.amplitude}{ the peak's greatest amplitude }
  \item{amplitude.range}{ the difference between the peak's  maximum amplitude and its minimum amplitude }
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
function (spg, passband, amplitude.cutoff) 
{
    freqs <- attr(spg, "frequency")
    g <- freqs > min(passband)
    below <- sum(!g)
    g <- g & freqs < max(passband)
    peak.values <- data.frame(t(apply(spg[, g], 1, collect.energy, 
        amplitude.cutoff)))
    names(peak.values) <- c("lo", "hi", "sum")
    peak.values[, 1:2] <- peak.values[, 1:2] + below
    peaks <- data.frame(time = attr(spg, "time"), min.frequency = freqs[peak.values$lo], 
        max.frequency = freqs[peak.values$hi], amplitude.sum = peak.values$sum)
    for (i in 1:nrow(peaks)) {
        ix <- peak.values$lo[i]:peak.values$hi[i]
        amps <- spg[i, ix]
        peaks$mean.frequency[i] <- weighted.mean(freqs[ix], amps)
        peaks$mean.amplitude[i] <- mean(amps)
        tmp <- range(amps)
        peaks$max.amplitude[i] <- tmp[2]
        peaks$amplitude.range[i] <- diff(tmp)
    }
    return(invisible(peaks))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sound }
