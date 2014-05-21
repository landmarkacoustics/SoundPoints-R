\name{make.tmp}
\alias{make.tmp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  appends the file name of the source file to a landmark data frame
}
\description{
  the fact that I don't even write "temp" out completely should be a clue...
}
\usage{
make.tmp(sp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{
    a landmark data frame
  }
}
\details{
  %%  ~~ If necessary, more details than the description above ~~
}
\value{
  a landmark data frame that includes the source file as a text column
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
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (sp) 
{
    tmp <- data.frame(file.name = sp$file, sp$landmarks)
    tmp$file.name <- as.character(tmp$file.name)
    invisible(tmp)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ morphometrics }
