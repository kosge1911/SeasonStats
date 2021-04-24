\name{standingplot}
\alias{standingplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot standings of the whole season
}
\description{
This function creates a plot with the standigs of every team in all rounds.
}
\usage{
standingplot(games, TEAM = NULL)
}
\arguments{
  \item{games}{Data frame with all played matches
}
  \item{TEAM}{Shows trend of a specific team. If no team is given, the trend of all teams will be shown. To see all possible teams use \bold{allteams(games)}
}
}

\author{
Georg Kosso
}
\note{
It will take around 30-50 seconds to show the trends of all teams!
}
\examples{
## Show trends of all teams
standingplot(GermanBundesliga)

## Show trend of one specific team
standingplot(GermanBundesliga, TEAM = "Werder Bremen")
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
