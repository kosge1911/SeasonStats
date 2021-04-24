\name{crosstab}
\alias{crosstab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Makes the crosstable of one season
}
\description{
This function generates a table with all results in shape of a crosstable
}
\usage{
crosstab(games)
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
\examples{
## Create the crosstable
crosstab(GermanBundesliga)
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
