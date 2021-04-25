###################################
# Code for the SeasonStats Package
###################################

#------------------------------------------------------------
# Included data sets
#------------------------------------------------------------

GermanBundesliga <- read.csv("de.1.csv", encoding = "UTF-8")
PremierLeague <- read.csv("eng.1.csv", encoding = "UTF-8")
PrimeraDivision <- read.csv("es.1.csv", encoding = "UTF-8")

#------------------------------------------------------------
# Function that shows you all teams
#------------------------------------------------------------

allteams <- function(games){
  print(unique(games$Team.1))
}

#------------------------------------------------------------
# Function for standings during and at the end of the season
#------------------------------------------------------------

league.table <- function(games, ROUND = (length(unique(games$Team.2)) - 1) * 2){

  # A function that creates the table of the season.
  # games ... dataframe with all games of one season
  # ROUND ... shows table of specific round
  # If ROUND is not given, then you receive the final table.

  if(ROUND > (length(unique(games$Team.2)) - 1) * 2){
    stop(paste("only ", " Rounds played in this season",
               sep = as.character((length(unique(games$Team.2)) - 1) * 2)))

    # Error, if too many rounds were given
  }

  # Data cleansing

  games$Round <- as.numeric(games$Round)
  games <- games[order(games$Round), ]

  temp <- as.character(games$FT)
  temp <- strsplit(temp, split = "-")
  temp <- unlist(temp)

  homegoal <- temp[c(TRUE, FALSE)]
  awaygoal <- temp[c(FALSE, TRUE)]

  games <- cbind(games, homegoal = as.numeric(homegoal), awaygoal = as.numeric(awaygoal))
  games$FT <- NULL

  # Create empty dataframe

  df <- data.frame(Position = integer(length(unique(games$Team.1))),
                 Team = character(length(unique(games$Team.1))),
                 Played = integer(length(unique(games$Team.1))),
                 Won = integer(length(unique(games$Team.1))),
                 Drawn = integer(length(unique(games$Team.1))),
                 Lost = integer(length(unique(games$Team.1))),
                 GoalsFor = integer(length(unique(games$Team.1))),
                 GoalsAgainst = integer(length(unique(games$Team.1))),
                 GoalDifference = integer(length(unique(games$Team.1))),
                 Points = integer(length(unique(games$Team.1))))
  df$Team <- unique(games$Team.1)

  # for-loop to compute the points of every team

  for(i in 1:(ROUND * (length(unique(games$Team.1)) / 2))){
    if(games$homegoal[i] > games$awaygoal[i]){
      df$Points[games$Team.1[i] == df$Team] <- df$Points[games$Team.1[i] == df$Team] + 3
      df$Won[games$Team.1[i] == df$Team] <- df$Won[games$Team.1[i] == df$Team] + 1
      df$Lost[games$Team.2[i] == df$Team] <- df$Lost[games$Team.2[i] == df$Team] + 1
    }
    if(games$homegoal[i] < games$awaygoal[i]){
      df$Points[games$Team.2[i] == df$Team] <- df$Points[games$Team.2[i] == df$Team] + 3
      df$Won[games$Team.2[i] == df$Team] <- df$Won[games$Team.2[i] == df$Team] + 1
      df$Lost[games$Team.1[i] == df$Team] <- df$Lost[games$Team.1[i] == df$Team] + 1
    }
    if(games$homegoal[i] == games$awaygoal[i]){
      (df$Points[games$Team.2[i] == df$Team] <- df$Points[games$Team.2[i] == df$Team] + 1) && (df$Points[games$Team.1[i] == df$Team] <- df$Points[games$Team.1[i] == df$Team] + 1)
      df$Drawn[games$Team.1[i] == df$Team] <- df$Drawn[games$Team.1[i] == df$Team] + 1
      df$Drawn[games$Team.2[i] == df$Team] <- df$Drawn[games$Team.2[i] == df$Team] + 1
    }
  }

  # for-loop to compute the Goals and played games

  for(i in 1:(ROUND * (length(unique(games$Team.1)) / 2))){
    df$GoalsFor[games$Team.1[i] == df$Team] <- df$GoalsFor[games$Team.1[i] == df$Team] + games$homegoal[i]
    df$GoalsFor[games$Team.2[i] == df$Team] <- df$GoalsFor[games$Team.2[i] == df$Team] + games$awaygoal[i]
    df$GoalsAgainst[games$Team.1[i] == df$Team] <- df$GoalsAgainst[games$Team.1[i] == df$Team] + games$awaygoal[i]
    df$GoalsAgainst[games$Team.2[i] == df$Team] <- df$GoalsAgainst[games$Team.2[i] == df$Team] + games$homegoal[i]
    df$Played[games$Team.1[i] == df$Team] <- df$Played[games$Team.1[i] == df$Team] + 1
    df$Played[games$Team.2[i] == df$Team] <- df$Played[games$Team.2[i] == df$Team] + 1
  }
  
  # Computing the Goaldifference
  
  for(i in 1:nrow(df)){
    df$GoalDifference <- df$GoalsFor - df$GoalsAgainst
  }

  # order the table and conditions for a tie in points
  
  position <- order(df$Points, df$GoalDifference, df$GoalsFor, decreasing = TRUE)
  df <- df[position, ]
  df$Position <- 1:length(unique(games$Team.1))
  rownames(df) <- NULL
  return(df)
}

#------------------------------------------------------------------
# Function to pick one team to see its stats
#------------------------------------------------------------------

team <- function(t, ROUND = (length(unique(data$Team.2)) - 1) * 2){

  # Error message if a wrong amount of rounds were passed

  if(ROUND > (length(unique(data$Team.2)) - 1) * 2){
    stop(paste("only ", " Rounds played in this season",
               sep = as.character((length(unique(data$Team.2)) - 1) * 2)))
  }

  # Using Fuzzy Matching
  temp <- unlist(lapply(t, agrep, x = unique(data$Team.1), value = TRUE, max = 1))

  res <- league(data, ROUND)[league(data, ROUND)$Team == temp, ]
  return(res)
}

#----------------------------------------------------------------
# Plot of table history during the whole season
#----------------------------------------------------------------

standingplotfull <- function(games, TEAM = NULL){

  # Function that creates the standings for every team in every round
  # games ... data frame with all games in one season
  # TEAM ... show the table history during the season of only one team
  # WARNING: It takes 30-50 seconds to create the plot

  dev.new() # Opening a new plot window
  
  # If no team was given
  if(is.null(TEAM)){
    # Creating an empty data frame with the team names as row names and as many columns as rounds played
    a <- data.frame(row.names = unique(games$Team.1))
    a[, ncol(a) + 1:((length(unique(games$Team.2)) - 1) * 2)] = NA
    
    # Fill the empty data frame with the positions of every team in every round
    for(j in 1:length(unique(games$Team.1))){
      for(i in 1:((length(unique(games$Team.2)) - 1) * 2)){
        a[j, i] <- league(games, i)$Position[league(games, i)$Team == rownames(a)[j]]
      }
    }
    
    # Colors for the teams 
    colorsall <- c("red", "green", "blue", "black", "yellow", "magenta", "grey", "orange", "cyan", "seagreen",
                   "darkred", "darkgreen", "darkblue", "yellow4", "darkmagenta", "darkgrey", "darkcyan", "darkseagreen",
                   "darkorange", "beige")
    
    # Plot the data
    par(mar = c(9, 4, 2, 2), xpd = TRUE)
    plot(1:((length(unique(games$Team.2)) - 1) * 2), 1:((length(unique(games$Team.2)) - 1) * 2), ylim = c(18, 1),
         type = "n", xlab = NA, ylab = "Position", xaxt = "n", yaxt = "n")
    axis(3, at = 1:((length(unique(games$Team.2)) - 1) * 2), cex.axis = 0.5)
    axis(2, at = 1:length(unique(games$Team.1)), cex.axis = 0.5)

    for(i in 1:length(unique(games$Team.1))){
      points(1:((length(unique(games$Team.2)) - 1) * 2), a[i, ], type = "o", col = colorsall[i],
             pch = 19, cex = 0.5)
    }
    legend(20, unique(games$Team.1), col = colorsall, pch = 20, ncol = 3, bty = "n")
  }
  
  # If a particular was given
  else{
    temp <- rep(0, (length(unique(games$Team.2)) - 1) * 2)
    par(mar = c(4, 4, 4, 4), xpd = TRUE)
    
    # Only compute the position in every round of one team
    for(i in 1:((length(unique(games$Team.2)) - 1) * 2)){
      temp[i] <- league(games, i)$Position[league(games, i)$Team == TEAM]
      plot(1:((length(unique(games$Team.2)) - 1) * 2), temp, type = "o", ylim = c(length(unique(games$Team.1)), 1),
           pch = 19, xaxt = "n", yaxt = "n", main = paste("Position", TEAM), xlab = "Round", ylab = "Position")
      axis(1, at = 1:((length(unique(games$Team.2)) - 1) * 2), cex.axis = 0.5)
      axis(2, at = 1:length(unique(games$Team.1)), cex.axis = 0.5)
    }
  }
}
#----------------------------------------------------------------
# Plot Leader - Last
#----------------------------------------------------------------

lplot <- function(games){

  # Gives you a visualization of the leader and the team on the last position.
  # games ... data frame with all matches
  
  # Compute the teams that are on the first position in every round
  temp <- rep(0, length(unique(games$Round)))
  for(i in 1:length(unique(games$Round))){
    temp[i] <- league(games, i)$Team[league(games, i)$Position == 1]
  }
  
  # Edit everything to create a barplot
  names(temp) <- temp
  temp1 <- rep(1, length(unique(games$Round)))
  names(temp1) <- names(temp)
  temp1 <- as.matrix(temp1)
  
  # Colors for the teams
  colorsall <- c("red", "green", "blue", "black", "yellow", "magenta", "grey", "orange", "cyan", "seagreen",
                 "darkred", "darkgreen", "darkblue", "yellow4", "darkmagenta", "darkgrey", "darkcyan", "darkseagreen",
                 "darkorange", "beige")
  
  # Loop that allocates the correct color to the teams of the vector created at the beginning
  farben <- data.frame(Teams = unique(games$Team.1), farben = colorsall[1:length(unique(games$Team.1))])
  teamfarben <- rep(0, max(unique(games$Round)))
  for(i in 1:length(unique(games$Team.1))){
    for(j in 1:length(unique(games$Round))){
      if(any(rownames(temp1)[j] == farben$Teams[i]) == TRUE){
        teamfarben[j] <- farben$farben[farben$Teams == farben$Teams[i]]
      }
      else{
        next
      }
    }
  }
  
  # Plot the vector with the fitting colors
  par(mar = c(5, 4, 4, 8), xpd = TRUE, mfrow = c(1, 2))
  barplot(temp1, beside = FALSE, col = teamfarben, ylim = NULL, border = NA, yaxt = "n", main = "Leader")
  axis(2, at = 1:max(unique(games$Round)), las = 1, cex.axis = 0.5)
  legend(max(unique(games$Round)), unique(rownames(temp1)), col = unique(teamfarben), pch = 19, cex = 0.6)

  # Compute the teams that are on the last position in every round
  temp2 <- rep(0, length(unique(games$Round)))
  for(i in 1:length(unique(games$Round))){
    temp2[i] <- league(games, i)$Team[league(games, i)$Position == length(unique(games$Team.1))]
  }
  names(temp2) <- temp2
  temp3 <- rep(1, length(unique(games$Round)))
  names(temp3) <- names(temp2)
  temp3 <- as.matrix(temp3)

  # Colors for the teams
  colorsall <- c("red", "green", "blue", "black", "yellow", "magenta", "grey", "orange", "cyan", "seagreen",
                 "darkred", "darkgreen", "darkblue", "yellow4", "darkmagenta", "darkgrey", "darkcyan", "darkseagreen",
                 "darkorange", "beige")
  
  # Loop that allocates the correct color to the teams of the vector created at the beginning
  farben <- data.frame(Teams = unique(games$Team.1), farben = colorsall[1:length(unique(games$Team.1))])
  teamfarben <- rep(0, max(unique(games$Round)))
  for(i in 1:length(unique(games$Team.1))){
    for(j in 1:length(unique(games$Round))){
      if(any(rownames(temp3)[j] == farben$Teams[i]) == TRUE){
        teamfarben[j] <- farben$farben[farben$Teams == farben$Teams[i]]
      }
      else{
        next
      }
    }
  }
  
  # Plot the vector with the fitting colors
  barplot(temp3, beside = FALSE, col = teamfarben, ylim = NULL, border = NA, yaxt = "n", main = "Last")
  axis(2, at = 1:max(unique(games$Round)), las = 1, cex.axis = 0.5)
  legend(max(unique(games$Round)), unique(rownames(temp3)), col = unique(teamfarben), pch = 19, cex = 0.6)
}

# ------------------------------------
# Cross table
# ------------------------------------

crosstab <- function(games){

  # Function that gives you the cross table of all matches
  # games ... data frame with all matches

  # Create a data frame with as many columns and rows as teams in the season
  d <- data.frame(row.names = unique(games$Team.1))
  d[, ncol(d) + 1:length(unique(games$Team.2))] = NA
  colnames(d) <- unique(games$Team.1)

  # Loops to find the matching teams and insert the result in the data frame
  for(j in 1:length(colnames(d))){
    for(k in 1:length(rownames(d))){
      for(i in 1:length(games$Team.1)){
        if(games$Team.1[i] == colnames(d)[j] & games$Team.2[i] == rownames(d)[k]){
          d[colnames(d)[j], rownames(d)[k]] <- games$FT[i]
        }
      }
    }
  }
  print(d)
}

#----------------------------------
# Home- and Awaytable
#----------------------------------


HAtable <- function(games, type = "Home"){

  # Function to create the home and away table
  # type can be "Home" and "away"
  # Very similar to league.table() function
  
  # Data cleansing
  games$Round <- as.numeric(games$Round)
  games <- games[order(games$Round), ]

  temp <- as.character(games$FT)
  temp <- strsplit(temp, split = "-")
  temp <- unlist(temp)

  homegoal <- temp[c(TRUE, FALSE)]
  awaygoal <- temp[c(FALSE, TRUE)]

  games <- cbind(games, homegoal = as.numeric(homegoal), awaygoal = as.numeric(awaygoal))
  games$FT <- NULL
  
  # Create empty data frame
  df <- data.frame(Position = integer(length(unique(games$Team.1))),
                   Team = character(length(unique(games$Team.1))),
                   Played = integer(length(unique(games$Team.1))),
                   Won = integer(length(unique(games$Team.1))),
                   Drawn = integer(length(unique(games$Team.1))),
                   Lost = integer(length(unique(games$Team.1))),
                   GoalsFor = integer(length(unique(games$Team.1))),
                   GoalsAgainst = integer(length(unique(games$Team.1))),
                   GoalDifference = integer(length(unique(games$Team.1))),
                   Points = integer(length(unique(games$Team.1))))
  df$Team <- unique(games$Team.1)
  
  # Point of view of the away team
  # Computing the points by loosing, winning or draw
  if(type == "Away"){
    for(i in 1:nrow(games)){
      if(games$homegoal[i] < games$awaygoal[i]){
        df$Points[games$Team.2[i] == df$Team] <- df$Points[games$Team.2[i] == df$Team] + 3
        df$Won[games$Team.2[i] == df$Team] <- df$Won[games$Team.2[i] == df$Team] + 1
        df$GoalsFor[games$Team.2[i] == df$Team] <- df$GoalsFor[games$Team.2[i] == df$Team] + games$awaygoal[i]
        df$GoalsAgainst[games$Team.2[i] == df$Team] <- df$GoalsAgainst[games$Team.2[i] == df$Team] + games$homegoal[i]
      }
      if(games$homegoal[i] == games$awaygoal[i]){
        df$Points[games$Team.2[i] == df$Team] <- df$Points[games$Team.2[i] == df$Team] + 1
        df$Drawn[games$Team.2[i] == df$Team] <- df$Drawn[games$Team.2[i] == df$Team] + 1
        df$GoalsFor[games$Team.2[i] == df$Team] <- df$GoalsFor[games$Team.2[i] == df$Team] + games$awaygoal[i]
        df$GoalsAgainst[games$Team.2[i] == df$Team] <- df$GoalsAgainst[games$Team.2[i] == df$Team] + games$homegoal[i]
      }
      if(games$homegoal[i] > games$awaygoal[i]){
        df$Lost[games$Team.2[i] == df$Team] <- df$Lost[games$Team.2[i] == df$Team] + 1
        df$GoalsFor[games$Team.2[i] == df$Team] <- df$GoalsFor[games$Team.2[i] == df$Team] + games$awaygoal[i]
        df$GoalsAgainst[games$Team.2[i] == df$Team] <- df$GoalsAgainst[games$Team.2[i] == df$Team] + games$homegoal[i]
      }
    }
    
    # Computing the goal difference
    for(i in 1:nrow(df)){
      df$GoalDifference <- df$GoalsFor - df$GoalsAgainst
    }
    
    # Order the table and conditions for a tie in points
    position <- order(df$Points, df$GoalDifference, df$GoalsFor, decreasing = TRUE)
    df <- df[position, ]
    df$Position <- 1:length(unique(games$Team.1))
    df$Played <- max(games$Round) / 2
    rownames(df) <- NULL
    return(df)
  }

  # Point of view of the home team
  # Computing the points by loosing, winning or draw
  else{
    for(i in 1:nrow(games)){
      if(games$homegoal[i] > games$awaygoal[i]){
        df$Points[games$Team.1[i] == df$Team] <- df$Points[games$Team.1[i] == df$Team] + 3
        df$Won[games$Team.1[i] == df$Team] <- df$Won[games$Team.1[i] == df$Team] + 1
        df$GoalsFor[games$Team.1[i] == df$Team] <- df$GoalsFor[games$Team.1[i] == df$Team] + games$homegoal[i]
        df$GoalsAgainst[games$Team.1[i] == df$Team] <- df$GoalsAgainst[games$Team.1[i] == df$Team] + games$awaygoal[i]
      }
      if(games$homegoal[i] == games$awaygoal[i]){
        df$Points[games$Team.1[i] == df$Team] <- df$Points[games$Team.1[i] == df$Team] + 1
        df$Drawn[games$Team.1[i] == df$Team] <- df$Drawn[games$Team.1[i] == df$Team] + 1
        df$GoalsFor[games$Team.1[i] == df$Team] <- df$GoalsFor[games$Team.1[i] == df$Team] + games$homegoal[i]
        df$GoalsAgainst[games$Team.1[i] == df$Team] <- df$GoalsAgainst[games$Team.1[i] == df$Team] + games$awaygoal[i]
      }
      if(games$homegoal[i] < games$awaygoal[i]){
        df$Lost[games$Team.1[i] == df$Team] <- df$Lost[games$Team.1[i] == df$Team] + 1
        df$GoalsFor[games$Team.1[i] == df$Team] <- df$GoalsFor[games$Team.1[i] == df$Team] + games$homegoal[i]
        df$GoalsAgainst[games$Team.1[i] == df$Team] <- df$GoalsAgainst[games$Team.1[i] == df$Team] + games$awaygoal[i]
      }
    }
    
    # Computing the goal difference
    for(i in 1:nrow(df)){
      df$GoalDifference <- df$GoalsFor - df$GoalsAgainst
    }
    
    # Order the table and conditions for a tie in points
    position <- order(df$Points, df$GoalDifference, df$GoalsFor, decreasing = TRUE)
    df <- df[position, ]
    df$Position <- 1:length(unique(games$Team.1))
    df$Played <- max(games$Round) / 2
    rownames(df) <- NULL
    return(df)
  }
}

#------------------------------------------------------------------------------------------
# Show relative frequencies of goals and compare with the Normal or Poisson distribution
#-----------------------------------------------------------------------------------------

Probgoalplot <- function(games, TEAM = NULL, distribution = NULL, in.match = FALSE){

  # This function shows the relative frequencies of the goals and can also compare them with
  # the Normal and Poisson distribution
  # games ... data frame with all games of one season
  # TEAM ... show relative frequencies of a particular team
  # distribution ... choose between "Poisson" and "Normal"
  # in.match ... if TRUE all goals of the matches were used


  # Required package
  library(ggplot2)

  # Data Cleansing

  games$Round <- as.numeric(games$Round)
  games <- games[order(games$Round), ]

  temp <- as.character(games$FT)
  temp <- strsplit(temp, split = "-")
  temp <- unlist(temp)

  homegoal <- temp[c(TRUE, FALSE)]
  awaygoal <- temp[c(FALSE, TRUE)]

  games <- cbind(games, homegoal = as.numeric(homegoal), awaygoal = as.numeric(awaygoal))
  games$FT <- NULL

  games$total <- games$homegoal + games$awaygoal


  if(in.match == TRUE){
    if(is.null(TEAM)){
      if(is.null(distribution)){

        # Create a gapless frequency table of the goals
        dist <- table(c(games$total))
        dist.full <- rep(0, max(games$total) + 1)
        names(dist.full) <- 0:max(games$total)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        # Create a data frame with the frequencies and relative frequencies
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)))
        rownames(data_freq) <- 0:max(games$total)

        # Plot the data frame
        ggplot(data_freq, aes(x = rownames(data_freq), y = Prob)) +
          geom_histogram(stat = "identity", fill = "blue") +
          labs(title = "Relative frequencies of goals in one Match", x = "Goals", y = "Frequency")

      } else if(isTRUE(distribution == "Poisson")){

        # Create a gapless frequency table of the goals
        dist <- table(c(games$total))
        dist.full <- rep(0, max(games$total) + 1)
        names(dist.full) <- 0:max(games$total)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        # Compute the weighted mean for the Poisson distribution
        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        # Compute Poisson distribution
        temp <- dpois(0:max(games$total), lambda = wmean)
        poi <- data.frame(Val = temp, row.names = 0:max(games$total))

        # Create a data frame with the frequencies and relative frequencies and the values of the Poisson distribution
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Poi = as.numeric(poi$Val))
        rownames(data_freq) <- 0:max(games$total)

        # Plot the data frame
        ggplot(data_freq, aes(x = rownames(data_freq), y = Poi, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(title = "Relative freqencies vs. Poisson Distribution", x = "Goals", y = "Probability") +
          scale_color_discrete(name = "", labels = c("RF", "Poi"))

      } else if(isTRUE(distribution == "Normal")){

        # Create a gapless frequency table of the goals
        dist <- table(c(games$total))
        dist.full <- rep(0, max(games$total) + 1)
        names(dist.full) <- 0:max(games$total)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        # Compute the weighted mean for the Normal distribution
        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        # Compute the weighted standard deviation
        wmean2 <- weighted.mean(as.numeric(names(dist.full))^2, w = dist.full)
        sig <- sqrt(wmean2 - wmean^2)

        # Compute Normal distribution
        temp <- dnorm(0:max(games$total), mean = wmean, sd = sig)
        norm <- data.frame(Val = temp, row.names = 0:max(games$total))

        # Create a data frame with the frequencies and relative frequencies and the values of the Normal distribution
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Norm = as.numeric(norm$Val))
        rownames(data_freq) <- 0:max(games$total)

        # Plot the data frame
        ggplot(data_freq, aes(x = rownames(data_freq), y = Norm, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(title = "Relative freqencies vs. Normal Distribution", x = "Goals", y = "Probability") +
          scale_color_discrete(name = "", labels = c("RF", "Norm"))
      }
    } else{
      stop("Can't use TEAM argument, when in.match = TRUE")
    }
  }
  else{
    if(is.null(TEAM)){

      if(is.null(distribution)){

        # Create a gapless frequency table of the goals
        dist <- table(c(games$awaygoal, games$homegoal))
        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        # Create a data frame with the frequencies and relative frequencies
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)

        # Plot the data frame
        ggplot(data_freq, aes(x = rownames(data_freq), y = Prob)) +
          geom_histogram(stat = "identity", fill = "blue") +
          labs(title = "Relative frequencies of goals socred per team in one Match", x = "Goals", y = "Frequency")

      } else if(isTRUE(distribution == "Normal")){

        # Create a gapless frequency table of the goals
        dist <- table(c(games$awaygoal, games$homegoal))
        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        # Compute the weighted mean for the Normal distribution
        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        # Compute the weighted standard deviation for the Normal distribution
        wmean2 <- weighted.mean(as.numeric(names(dist.full))^2, w = dist.full)
        sig <- sqrt(wmean2 - wmean^2)

        # Compute Normal distribution
        temp <- dnorm(0:max(games$awaygoal, games$homegoal), mean = wmean, sd = sig)
        norm <- data.frame(Val = temp, row.names = 0:max(games$awaygoal, games$homegoal))

        # Create a data frame with the frequencies and relative frequencies and the values of the Normal distribution
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Norm = as.numeric(norm$Val))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)

        # Plot the data frame
        ggplot(data_freq, aes(x = rownames(data_freq), y = Norm, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(title = "Relative freqencies vs. Normal Distribution", x = "Goals", y = "Frequency") +
          scale_color_discrete(name = "", labels = c("RF", "Norm"))
      }
      else if(isTRUE(distribution == "Poisson")){

        # Create a gapless frequency table of the goals
        dist <- table(c(games$awaygoal, games$homegoal))
        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        # Compute the weighted mean for the Poisson distribution
        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        # Compute Poisson distribution
        temp <- dpois(0:max(games$awaygoal, games$homegoal), lambda = wmean)
        poi <- data.frame(Val = temp, row.names = 0:max(games$awaygoal, games$homegoal))

        # Create a data frame with the frequencies and relative frequencies and the values of the Poisson distribution
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Poi = as.numeric(poi$Val))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)

        # Plot the data frame
        ggplot(data_freq, aes(x = rownames(data_freq), y = Poi, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(title = "Density of Goals scored per Team in each Match", x = "Goals", y = "Probability") +
          scale_color_discrete(name = "", labels = c("Poi", "Mean"))
      }
    }
    else {
      if(isTRUE(distribution == "Poisson")){

        # Create a gapless frequency table of the goals in relation to one particular team
        dist <- table(c(games$homegoal[games$Team.1 == TEAM], games$awaygoal[games$Team.2 == TEAM]))
        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        # Compute the weighted mean for the Poisson distribution
        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        # Compute Poisson distribution
        temp <- dpois(0:max(games$awaygoal, games$homegoal), lambda = wmean)
        poi <- data.frame(Val = temp, row.names = 0:max(games$awaygoal, games$homegoal))

        # Create a data frame with the frequencies and relative frequencies and the values of the Poisson distribution
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Poi = as.numeric(poi$Val))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)

        # Plot the data frame
        ggplot(data_freq, aes(x = rownames(data_freq), y = Poi, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(title = "Relative freqencies vs. Poisson Distribution", x = "Goals", y = "Frequency") +
          scale_color_discrete(name = "", labels = c("RF", "Poi"))

      } else if(isTRUE(distribution == "Normal")){

        # Create a gapless frequency table of the goals in relation to one particular team
        dist <- table(c(games$homegoal[games$Team.1 == TEAM], games$awaygoal[games$Team.2 == TEAM]))
        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        # Compute the weighted mean for the Normal distribution
        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        # Compute the weighted standard deviation for the Normal distribution
        wmean2 <- weighted.mean(as.numeric(names(dist.full))^2, w = dist.full)
        sig <- sqrt(wmean2 - wmean^2)

        # Compute Normal distribution
        temp <- dnorm(0:max(games$awaygoal, games$homegoal), mean = wmean, sd = sig)
        norm <- data.frame(Val = temp, row.names = 0:max(games$awaygoal, games$homegoal))

        # Create a data frame with the frequencies and relative frequencies and the values of the Normal distribution
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Norm = as.numeric(norm$Val))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)

        # Plot the data frame
        ggplot(data_freq, aes(x = rownames(data_freq), y = Norm, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(title = paste("Relative frequencies vs. Normal Distribution -", TEAM, sep = " "), x = "Goals", y = "Probability") +
          scale_color_discrete(name = "", labels = c("RF", "Norm"))
      }
      else{

        # Create a gapless frequency table of the goals in relation to one particular team
        dist <- table(c(games$homegoal[games$Team.1 == TEAM], games$awaygoal[games$Team.2 == TEAM]))
        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        # Create a data frame with the frequencies and relative frequencies
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)

        # Plot the data frame
        ggplot(data_freq, aes(x = rownames(data_freq), y = Prob)) +
          geom_histogram(stat = "identity", fill = "blue") +
          labs(title = paste("Relative frequencies of Goals:", TEAM, sep = " "), x = "Goals", y = "Frequency")
      }
    }
  }
}

allteams <- function(games){
  print(unique(games$Team.1))
}
GermanBundesliga <- read.csv("de.1.csv", encoding = "UTF-8") # German Bundesliga Season 2019/20
PremierLeague <- read.csv("eng.1.csv", encoding = "UTF-8") # Premier League Season 2017/18
PrimeraDivision <- read.csv("es.1.csv", encodin = "UTF-8") # Premiera Division Season 2017/18

Seasons <- function(){
  
  # Shows the included data sets
  
  print("German Bundesliga Season 2019/20 as GermanBundesliga")
  print("Premier League Season 2017/18 as PremierLeague")
  print("remiera Division Season 2017/18 as PrimeraDivision")
}
