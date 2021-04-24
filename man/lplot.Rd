\name{lplot}
\alias{lplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of leader and last in league
}
\description{
This function creates a plot that shows the leader and the last team in every Round.
}
\usage{
lplot(games)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{games}{
Data frame with all played matches
}
}

\author{
Georg Kosso
}
\note{
The colors may not match with the traditional colors of the team
}

\examples{
## Create plot
lplot(GermanBundesliga)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
