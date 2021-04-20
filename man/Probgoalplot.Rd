\name{Probgoalplot}
\alias{Probgoalplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distribution of goals
}
\description{
This function creates a plot of the distribution of goals scored in each match
}
\usage{
Probgoalplot(games, TEAM = NULL, distribution = NULL, in.match = FALSE)
}
\arguments{
  \item{games}{
Data frame with all played matches
}
  \item{TEAM}{
Shows the distribution of goals of one specific team. To see all possible teams use \bold{allteams(games)}
}
  \item{distribution}{
Choose between "Poisson" and "Normal" to approximate the distribution
}
  \item{in.match}{
If TRUE, then all goals in one match will be used. If FALSE, the goal frequencies of every team will be uesd.
}
}

\value{
  \item{distribution}{
The goals tend to be Normal or Poisson distributed. To use the distribution argument you can compare the actioal distribution with the Poisson or Normal distribution. The parameter lambda of the poisson distribution is the weighted mean of the goals. Same procedure with the parameter mu of the normal distribution. For sigma, the weighted standard deviation of the goals is used.
If no distribution given, then you recieve a barplot with the probabilities.
}
  \item{in.match}{
TRUE means all goals in one match. E.g. Team A vs. Team B ends 2-1, then we have three goals. If in.match = FALSE, then the goals of every Team for each match will be used. E.g. Team A vs. Team B ends 2-1, we have Team A two goals and Team B one goal. So if you use in.match = TRUE, you cannot select a specific team.
}
}
\author{
Georg Kosso
}

\examples{
## load all games of the Bundesliga Season of 2019/20
data <- read.csv("de.1.csv", encoding = "UTF-8")

## Create barplot of probabilites of goals scored per team in each match
Probgoalplot(data)

## Create barplot of probabilities of goals scored in each match
Probgoalplot(data, in.match = TRUE)

## Compare the Poisson distribution with the actual distribution of goals scored in each match
Probgoalplot(data, distribution = "Poisson", in.match = TRUE)

## Compare the Normal distribution with the actual distribution of goals scored by FC Augsburg
Probgoalplot(data, TEAM = "FC Augsburg", distribution = "Normal")

## Create barplot of probabilities of goals scored by Eintracht Frankfurt
Probgoalplot(data, TEAM = "Eintracht Frankfurt")
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
