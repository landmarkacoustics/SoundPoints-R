\name{SoundPoints-package}
\alias{SoundPoints-package}
\alias{SoundPoints}
\docType{package}
\title{
  Extracts acoustic landmarks from whistled sounds.
}
\description{
  This is a simple implementation of an algorithm for converting whistled
  sounds into vectors of time, frequency, and amplitude data that describe
  the contour of each whistle. The user determines the settings for
  separating whistles from background noise, as well as the number of
  landmarks to use when describing each sound.
}
\details{
  \tabular{ll}{
    Package: \tab SoundPoints\cr
    Type: \tab Package\cr
    Version: \tab 1.0\cr
    Date: \tab 2014-04-10\cr
    License: \tab GPL (>= 2)\cr
  }
  \tabular{ll}{
    soundpoints \tab reads a sound file and produces a spectrogram,
    high-resolution frequency contours, and a data frame of landmark
    data\cr
  }  
}
\author{
  Benjamin N. Taft \email{ben.taft@landmarkacoustics.com}

  Maintainer: Landmark Acoustics LLC \email{soundpoints@landmarkacoustics.com}
}
\references{
  http://scholarworks.umass.edu/cgi/viewcontent.cgi?article=1360&context=open_access_dissertations
}

\keyword{ package }
\keyword{ sound }
\keyword{ morphometrics}

\seealso{
  \code{\link[tuneR:tuneR-package]{tuneR}} ~~
}
\examples{
%~~ simple examples of the most important functions ~~
}
