\name{arsmooth}
\alias{arsmooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  compute an autoregressive smooth within each frequency bin of a spectrogram
}
\description{
  this function takes each horizontal part (actually columns in R) of a
  spectrogram and smooths it over time using autocorrelation
}
\usage{
arsmooth(spg, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spg}{
    a spectrogram
  }
  \item{\dots}{
    arguments to \code{ar}
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  a new spectrogram
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
  \code{\link{ar}}
  \code{\link{spectrogram}}
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (spg, ...) 
{
    if (!("spg" \%in\% class(spg))) 
        return(invisible(spg))
    cix <- ncol(spg):1
    for (i in 1:nrow(spg)) {
        A <- ar(ts(spg[i, cix]), ...)
        smoov <- spg[i, cix] - A$resid
        spg[i, cix] <- smoov
    }
    invisible(spg)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ autocorrelation }
\keyword{ spectrogram }% __ONLY ONE__ keyword per line
