\name{fast.soundpoints}
\alias{fast.soundpoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  a fast and dumbed-down version of soundpoints
}
\description{
  this version is faster because it only returns the landmark data frame, not the peaks or the spectrogram. it also makes some stupid assumptions about the sample rate of the sound, don't use it if it's not 44.1 kHz.
}
\usage{
fast.soundpoints(file.name,
 passband,
 amplitude.cutoff,
 loudness.column,
 half.life,
 var.columns,
 var.weights,
 n.landmarks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file.name}{
    the path to the sound file
  }
  \item{passband}{
    a two-element vector specifying the frequency passband
  }
  \item{amplitude.cutoff}{
    the total amplitude under a peak that determines its frequency range
  }
  \item{loudness.column}{
    the column of the peak matrix that corresponds to the amplitude you want
    to use
  }
  \item{half.life}{
    the decay rate of the weighted amplitude average used to discriminate
    between signal and noise
  }
  \item{var.columns}{
    the columns from the peak matrix that will be used to calculate landmarks
  }
  \item{var.weights}{
    the weights from \code{var.columns} that determine event
    detection. basically, you should write it as 1.0 / (critical differences).
  }
  \item{n.landmarks}{
    the number of landmarks in each set
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{file.name}{ the name, without path, of the source file }
  \item{landmarks}{ a data frame of landmarks, with one row per event}
}
\references{
  %% ~put references to the literature/web site here ~
}
\author{
  Benjamin N. Taft \email{ben.taft@landmark.acoustics.com}
}
\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{soundpoints}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (file.name, passband, amplitude.cutoff, loudness.column, 
    half.life, var.columns, var.weights, n.landmarks) 
{
    spg <- spectrogram(file.name, 107/44100, 100)
    peaks <- make.peaks(spg, passband, amplitude.cutoff)
    initial.amplitude <- quantile(peaks$mean.amplitude, 0.85)
    amplitude.threshold <- log(10)
    time.res <- get.time.res(spg)
    rm(spg)
    peaks$smooth.amplitude <- smooth.average(peaks[, loudness.column], 
        time.res, half.life, initial.amplitude)
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
    answer <- list(file = basename(file.name), landmarks = landmarks)
    return(invisible(answer))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
