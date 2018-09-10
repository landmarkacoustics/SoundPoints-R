\name{cepstrum}
\alias{cepstrum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Quefrency Cepstrum from Spectrogram
}
\description{
  uses algorithms from wikipedia and my rudimentary understanding of
  cepstra to convert a spectrogram into a cepstrum
}
\usage{
cepstrum(spg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spg}{
a spectrogram object
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  a cepstrum object, which can be plotted like a spectrogram
}
\references{
  http://en.wikipedia.org/wiki/Cepstrum
}
\author{
  Benjamin N. Taft \email{ben.taft@landmarkacoustics.com}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (spg) 
{
    if (is(spg) == "spg") {
        sample.rate <- sample.rate.spg(spg)
        NR <- nrow(spg)
        ix <- (ncol(spg)/2):1
        cpg <- matrix(NA, NR, length(ix))
        H <- hamming(ncol(spg))
        for (i in 1:NR) {
            cpg[i, ] <- spk(scale(spg[i, ], scale = F) * H)
        }
        attributes(cpg) <- c(attributes(cpg), list(time = attr(spg, 
            "time"), quefrency = sample.rate/ix))
        class(cpg) <- c("cepstrum", class(cpg))
        invisible(cpg)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ signal processing }
\keyword{ cepstrum }% __ONLY ONE__ keyword per line
