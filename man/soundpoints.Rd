\name{soundpoints}
\alias{soundpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  calculates acoustic landmarks for whistled sounds from an audio file
}
\description{
  each event in the sound file is represented by the same number of landmarks.
}
\usage{
soundpoints(file.name, time.res, freq.res, passband, amplitude.cutoff, loudness.column, half.life, initial.amplitude, amplitude.threshold, var.columns, var.weights, n.landmarks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{
    the location of a file in WAV format
  }
  \item{time.res}{
    the desired time resolution of the spectrogram
  }
  \item{freq.res}{
    the desired frequency resolution of the spectrogram -- this will be
    converted to the nearest power of two, based upon the sample rate of the
    input file
  }
  \item{passband}{
    a two-element vector that defines the frequency range within the
    spectrogram that will contain the landmarks
  }
  \item{amplitude.cutoff}{
    the area under the peak at each instant of the spectrogram
  }
  \item{loudness.column}{
    the variable to use when separating signal vs. noise. see \code{\link{make.peaks}}
  }
  \item{half.life}{
    this tunes the sensitivity of the amplitude smoother. smaller values
    means the moving average is more sensitive to sudden changes in
    amplitude. a good starting guess for this parameter is the time from one
    element to the next.
  }
  \item{initial.amplitude}{
    the starting amplitude of the signal vs. noise smooth. Large values
    exclude more noise at the beginning
  }
  \item{amplitude.threshold}{
    peaks are only counted as signal if they exceed the moving average by at
    least this much. If you set it at -1, for example, everything at least
    10 dB quieter than the moving average will be included.
  }
  \item{var.columns}{
    the columns of the frequency contour that will be included in the
    landmark analysis. see \code{\link{make.peaks}}.
  }
  \item{var.weights}{
    when calculating the difference between successive peaks, these are the
    weights of each of the \code{var.columns}. For example, you could
    include time, mean frequency, and mean amplitude as your variables, and set
    \code{var.weights = 1/c(0.1,4000,Inf)}. This would cause the event
    detector to ignore amplitude and make a new event when successive peaks
    are separated by 0.1 seconds or 4 kiloHertz.
  }
  \item{n.landmarks}{
    the number of landmarks per event. Events that include fewer than
    \code{n.landmarks} peaks will be thrown out.
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{file}{ the name of the audio file, without path information }
  \item{spg}{ the spectrogram object showing the whole file }
  \item{peaks}{ the frequency contour of the whole file }
  \item{landmarks}{ a data frame with one row per event, holding
    envelope and landmark measurements (e.g. duration, mean time, time1,
    time2, etc.)}
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

\section{Warning}{
  This function makes a spectrogram from the WHOLE file, so don't use it
  on long recordings!
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{detect.events}}
  \code{\link{make.peaks}}
  \code{\link{smooth.average}}
  \code{\link{plot.spg}}
  \code{\link{landmark.plot}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file.name, time.res, freq.res, passband, amplitude.cutoff, 
    loudness.column, half.life, initial.amplitude, amplitude.threshold, 
    var.columns, var.weights, n.landmarks) 
{
    spg <- spectrogram(file.name, time.res, freq.res)
    peaks <- make.peaks(spg, passband, amplitude.cutoff)
    peaks$smooth.amplitude <- smooth.average(peaks[, loudness.column], 
        get.time.res(spg), half.life, initial.amplitude)
    not.noise <- peaks[, loudness.column] > peaks$smooth.amplitude + 
        amplitude.threshold
    ids <- data.frame(event = rep(NA, nrow(peaks)), landmark = NA)
    ids$event[not.noise] = detect.events(peaks[not.noise, var.columns], 
        var.weights)
    ids$landmark[not.noise] <- assign.to.landmarks(ids$event[not.noise], 
        n.landmarks)
    no.smooth <- -ncol(peaks)
    landmarks <- aggregate(peaks[not.noise, no.smooth], list(event = ids$event[not.noise]), 
        mean)
    names(landmarks)[-1] <- paste("mean", names(landmarks)[-1], 
        sep = ".")
    landmarks$duration <- aggregate(peaks$time[not.noise], list(event = ids$event[not.noise]), 
        function(x) {
            diff(range(x))
        })$x
    for (j in 1:n.landmarks) {
        f <- not.noise & ids$landmark == j
        if (any(f)) {
            temp <- aggregate(peaks[f, no.smooth], list(event = ids$event[f]), 
                mean)
            names(temp)[-1] <- paste(names(temp)[-1], j, sep = ".")
            landmarks <- cbind(landmarks, temp[match(landmarks$event, 
                temp$event), -1])
        }
    }
    landmarks <- landmarks[is.finite(apply(landmarks, 1, sum)), 
        ]
    ix <- grep("time", names(landmarks))[-1]
    landmarks[, ix] <- landmarks[, ix] - landmarks$mean.time
    peaks$event <- ids$event
    peaks$landmark <- ids$landmark
    answer <- list(file = basename(file.name), spg = spg, peaks = peaks, 
        landmarks = landmarks)
    return(invisible(answer))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sound }
\keyword{ morphometrics }% __ONLY ONE__ keyword per line
