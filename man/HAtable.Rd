\name{HAtable}
\alias{HAtable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Home- or awaytable of the season
}
\description{
This function creates the home- or awaytable of one season
}
\usage{
HAtable(games, type = "Home")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{games}{
Data frame with all played matches
}
  \item{type}{
Choose between "Home", to show the hometable or "Away", to show the awaytable
}
}

\author{
Georg Kosso
}
\examples{

## Create the home table of the German Bundesliga
HAtable(GermanBundesliga, type = "Home")

## Create the away table
HAtable(German Bundesliga, type = "away")
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
