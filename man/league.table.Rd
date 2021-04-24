\name{league.table}
\alias{league.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Creating the table of a season
}
\description{
Creates a the table based on all played matches. You can also pick one special round to see the standing at one selected round.
}
\usage{
league.table(games, ROUND = "Last Round")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{games}{Data frame with all played matches.
%%     ~~Describe \code{x} here~~
}
  \item{ROUND}{Shows table at a selected round. If no value is given, then the final table will be shown.}
}


\author{
Georg Kosso
}

\examples{

## table after 18 rounds
league.table(GermanBudesliga, ROUND = 18)

## final table
league.table(data)

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
