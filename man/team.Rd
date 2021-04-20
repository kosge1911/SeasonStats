\name{team}
\alias{team}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selects one team of the table to see its values in the table
}
\description{
Shows the wins, draws, losts of one team. You can also use \bold{league(data)[league(data)$Team == "Team", ]}
}
\usage{
team(t, ROUND = "Last Round")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{t}{Team to be shown. To see all possible teams use \bold{allteams(games)}
%%     ~~Describe \code{x} here~~
}
  \item{ROUND}{Shows values of at one selected round. If no value is given, then the final tablevalues will be shown.}
}

\author{
Georg Kosso
}

\examples{
## load all games of the Bundesliga Season of 2019/20
data <- read.csv("de.1.csv", encoding = "UTF-8")

## Show values of Borussia Dortmund after 18 Rounds
team("Borussia Dortmund", 18)

## Show values of 1. FC Union Berlin after all games
team("1. FC Union Berlin")
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
