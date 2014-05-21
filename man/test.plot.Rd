\name{test.plot}
\alias{test.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  a rapid method for viewing the results of a call to \code{soundpoints}
}
\description{
this gives a quick-and-dirty way to check if your settings are
reasonable. you're going to call this A LOT
}
\usage{
test.plot(sp, ix, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{
    a list from a call to \code{soundpoints}
}
  \item{ix}{
    a vector of indices that describes the spacing between columns that
    point to comparable variables in a landmark matrix (e.g. time1,time2, etc.)
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
  \code{\link{soundpoints}}
  \code{\link{plot.spg}}
  \code{\link{landmark.plot}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sp, ix, ...) 
{
    par(mar = c(10, 10, 2, 2)/3, mgp = 2 * c(3, 1, 0)/3)
    plot(sp$spg, ...)
    landmark.plot(sp$landmarks, ix)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ sound }
\keyword{ morphometrics }% __ONLY ONE__ keyword per line
