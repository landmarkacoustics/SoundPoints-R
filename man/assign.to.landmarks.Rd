\name{assign.to.landmarks}
\alias{assign.to.landmarks}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  split events into landmarks
}
\description{
creates a factor of within-event landmark assignments given a factor of
event ids and a number of landmarks per event, in preparation for aggregation.
}
\usage{
assign.to.landmarks(ids, n.landmarks)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ids}{
    a factor uniquely identifying each event, e.g. \code{111223333334444}
}
  \item{n.landmarks}{
the number of landmarks per event
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  a vector of landmark identities, e.g. \code{112121112221212}
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (ids, n.landmarks) 
{
    landmarks <- rep(NA, length(ids))
    for (e in sort(unique(ids))) {
        fe <- ids == e & is.finite(ids)
        n <- sum(fe)
        landmarks[fe] <- ceiling(n.landmarks * (1:n)/n)
    }
    return(invisible(landmarks))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ morphometrics }

