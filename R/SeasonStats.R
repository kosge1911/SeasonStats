league <- function(games, ROUND = (length(unique(games$Team.2)) - 1) * 2){

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

  # Points, and game outcome

  for(i in 1:(ROUND * (length(unique(games$Team.1)) / 2))){
    if(games$homegoal[i] > games$awaygoal[i]){
      df$Points[games$Team.1[i] == df$Team] <- df$Points[games$Team.1[i] == df$Team] + 3
      df$Won[games$Team.1[i] == df$Team] <- df$Won[games$Team.1[i] == df$Team] + 1
      df$Lost[games$Team.2[i] == df$Team] <- df$Lost[games$Team.2[i] == df$Team] + 1
      # df$GoalsFor[games$Team.1[i] == df$Team] <- df$GoalsFor[games$Team.1[i] == df$Team] + games$homegoal[i]
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

  # Goals and played games

  for(i in 1:(ROUND * (length(unique(games$Team.1)) / 2))){
    df$GoalsFor[games$Team.1[i] == df$Team] <- df$GoalsFor[games$Team.1[i] == df$Team] + games$homegoal[i]
    df$GoalsFor[games$Team.2[i] == df$Team] <- df$GoalsFor[games$Team.2[i] == df$Team] + games$awaygoal[i]
    df$GoalsAgainst[games$Team.1[i] == df$Team] <- df$GoalsAgainst[games$Team.1[i] == df$Team] + games$awaygoal[i]
    df$GoalsAgainst[games$Team.2[i] == df$Team] <- df$GoalsAgainst[games$Team.2[i] == df$Team] + games$homegoal[i]
    df$Played[games$Team.1[i] == df$Team] <- df$Played[games$Team.1[i] == df$Team] + 1
    df$Played[games$Team.2[i] == df$Team] <- df$Played[games$Team.2[i] == df$Team] + 1
  }

  for(i in 1:nrow(df)){
    df$GoalDifference <- df$GoalsFor - df$GoalsAgainst
  }

  position <- order(df$Points, df$GoalDifference, df$GoalsFor, decreasing = TRUE)
  df <- df[position, ]
  df$Position <- 1:length(unique(games$Team.1))
  return(df)
  # print(paste("Table after ", " Round(s)", sep = as.character(ROUND)))
}
team <- function(t, ROUND = (length(unique(data$Team.2)) - 1) * 2){

  if(ROUND > (length(unique(data$Team.2)) - 1) * 2){
    stop(paste("only ", " Rounds played in this season",
               sep = as.character((length(unique(data$Team.2)) - 1) * 2)))
  }

  temp <- unlist(lapply(t, agrep, x = unique(data$Team.1), value = TRUE, max = 1))

  res <- league(data, ROUND)[league(data, ROUND)$Team == temp, ]
  return(res)
}
standingplot <- function(games, TEAM = NULL){

  # Function that creates the standings for every team in every round
  # games ... dataframe with all games in one season
  # TEAM ... show the standig during the season of only one TEAM

  dev.new()
  if(is.null(TEAM)){
    a <- data.frame(row.names = unique(games$Team.1))
    a[, ncol(a) + 1:((length(unique(games$Team.2)) - 1) * 2)] = NA
    for(j in 1:length(unique(games$Team.1))){
      for(i in 1:((length(unique(games$Team.2)) - 1) * 2)){
        a[j, i] <- league(games, i)$Position[league(games, i)$Team == rownames(a)[j]]
      }
    }
    colorsall <- c("red", "green", "blue", "black", "yellow", "magenta", "grey", "orange", "cyan", "seagreen",
                   "darkred", "darkgreen", "darkblue", "yellow4", "darkmagenta", "darkgrey", "darkcyan", "darkseagreen",
                   "darkorange", "beige")
    par(mar = c(10, 4, 5, 2), xpd = TRUE)
    plot(1:((length(unique(games$Team.2)) - 1) * 2), 1:((length(unique(games$Team.2)) - 1) * 2), ylim = c(length(unique(games$Team.1)), 1),
         type = "n", xlab = NA, ylab = "Position", xaxt = "n", yaxt = "n", main = "Round")
    axis(3, at = 1:((length(unique(games$Team.2)) - 1) * 2), cex.axis = 0.5)
    axis(2, at = 1:length(unique(games$Team.1)), cex.axis = 0.5)
    legend(21, unique(games$Team.1), col = colorsall, pch = 20, ncol = 3, bty = "n")
    par(xpd = FALSE)
    abline(h = 1:length(unique(games$Team.1)), col = "grey")
    for(i in 1:length(unique(games$Team.1))){
      points(1:((length(unique(games$Team.2)) - 1) * 2), a[i, ], type = "o", col = colorsall[i],
             pch = 19, cex = 0.75)
    }
  }
  else{
    temp <- rep(0, (length(unique(games$Team.2)) - 1) * 2)
    par(mar = c(4, 4, 4, 4), xpd = FALSE)
    for(i in 1:((length(unique(games$Team.2)) - 1) * 2)){
      temp[i] <- league(games, i)$Position[league(games, i)$Team == TEAM]
      plot(1:((length(unique(games$Team.2)) - 1) * 2), temp, type = "o", ylim = c(length(unique(games$Team.1)), 1),
           pch = 19, xaxt = "n", yaxt = "n", main = paste("Position", TEAM), xlab = "Round", ylab = "Position")
      axis(1, at = 1:((length(unique(games$Team.2)) - 1) * 2), cex.axis = 0.5)
      axis(2, at = 1:length(unique(games$Team.1)), cex.axis = 0.5)
      abline(h = 1:length(unique(games$Team.1)))
    }
  }
}
lplot <- function(games){

  # Gives you a visualization of the leader and the team on the last position.
  # games ... dataframe with all matches

  temp <- rep(0, length(unique(games$Round)))
  for(i in 1:length(unique(games$Round))){
    temp[i] <- league(games, i)$Team[league(games, i)$Position == 1]
  }
  names(temp) <- temp
  temp1 <- rep(1, length(unique(games$Round)))
  names(temp1) <- names(temp)
  temp1 <- as.matrix(temp1)
  colorsall <- c("red", "green", "blue", "black", "yellow", "magenta", "grey", "orange", "cyan", "seagreen",
                 "darkred", "darkgreen", "darkblue", "yellow4", "darkmagenta", "darkgrey", "darkcyan", "darkseagreen",
                 "darkorange", "beige")
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
  par(mar = c(1, 4, 4, 8), xpd = TRUE, mfrow = c(1, 2))
  barplot(temp1, beside = FALSE, col = teamfarben, ylim = NULL, border = NA, yaxt = "n", main = "Leader")
  axis(2, at = 1:max(unique(games$Round)), las = 1, cex.axis = 0.5)
  legend(max(unique(games$Round)), unique(rownames(temp1)), col = unique(teamfarben), pch = 19, cex = 0.6)

  temp2 <- rep(0, length(unique(games$Round)))
  for(i in 1:length(unique(games$Round))){
    temp2[i] <- league(games, i)$Team[league(games, i)$Position == length(unique(games$Team.1))]
  }
  names(temp2) <- temp2
  temp3 <- rep(1, length(unique(games$Round)))
  names(temp3) <- names(temp2)
  temp3 <- as.matrix(temp3)

  colorsall <- c("red", "green", "blue", "black", "yellow", "magenta", "grey", "orange", "cyan", "seagreen",
                 "darkred", "darkgreen", "darkblue", "yellow4", "darkmagenta", "darkgrey", "darkcyan", "darkseagreen",
                 "darkorange", "beige")
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
  barplot(temp3, beside = FALSE, col = teamfarben, ylim = NULL, border = NA, yaxt = "n", main = "Last")
  axis(2, at = 1:max(unique(games$Round)), las = 1, cex.axis = 0.5)
  legend(max(unique(games$Round)), unique(rownames(temp3)), col = unique(teamfarben), pch = 19, cex = 0.6)
}
crosstab <- function(games){

  # Functon that gives you the crosstable of all matches
  # games ... dataframe with all matches

  d <- data.frame(row.names = unique(games$Team.1))
  d[, ncol(d) + 1:length(unique(games$Team.2))] = NA
  colnames(d) <- unique(games$Team.1)

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
HAtable <- function(games, type = "Home"){

  games$Round <- as.numeric(games$Round)
  games <- games[order(games$Round), ]

  temp <- as.character(games$FT)
  temp <- strsplit(temp, split = "-")
  temp <- unlist(temp)

  homegoal <- temp[c(TRUE, FALSE)]
  awaygoal <- temp[c(FALSE, TRUE)]

  games <- cbind(games, homegoal = as.numeric(homegoal), awaygoal = as.numeric(awaygoal))
  games$FT <- NULL

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
    for(i in 1:nrow(df)){
      df$GoalDifference <- df$GoalsFor - df$GoalsAgainst
    }
    position <- order(df$Points, df$GoalDifference, df$GoalsFor, decreasing = TRUE)
    df <- df[position, ]
    df$Position <- 1:length(unique(games$Team.1))
    df$Played <- 17
    return(df)
  }

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
    for(i in 1:nrow(df)){
      df$GoalDifference <- df$GoalsFor - df$GoalsAgainst
    }
    position <- order(df$Points, df$GoalDifference, df$GoalsFor, decreasing = TRUE)
    df <- df[position, ]
    df$Position <- 1:length(unique(games$Team.1))
    df$Played <- max(games$Round) / 2
    return(df)
  }
}
Probgoalplot <- function(games, TEAM = NULL, distribution = NULL, in.match = FALSE){
  library(ggplot2)

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
        dist <- table(c(games$total))
        dist
        dist.full <- rep(0, max(games$total) + 1)
        names(dist.full) <- 0:max(games$total)

        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)))
        rownames(data_freq) <- 0:max(games$total)

        ggplot(data_freq, aes(x = rownames(data_freq), y = Prob)) +
          geom_histogram(stat = "identity", fill = "blue") +
          labs(title = "Goals scored per Team in each Match", x = "Goals", y = "Probability")

      } else if(isTRUE(distribution == "Poisson")){
        dist <- table(c(games$total))
        dist
        dist.full <- rep(0, max(games$total) + 1)
        names(dist.full) <- 0:max(games$total)

        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        temp <- dpois(0:max(games$total), lambda = wmean)

        poi <- data.frame(Val = temp, row.names = 0:max(games$total))

        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Poi = as.numeric(poi$Val))
        rownames(data_freq) <- 0:max(games$total)

        ggplot(data_freq, aes(x = rownames(data_freq), y = Poi, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(x = "Goals", y = "Probability") +
          scale_color_discrete(name = "", labels = c("Poi", "Mean"))

      } else if(isTRUE(distribution == "Normal")){
        dist <- table(c(games$total))
        dist
        dist.full <- rep(0, max(games$total) + 1)
        names(dist.full) <- 0:max(games$total)

        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        wmean2 <- weighted.mean(as.numeric(names(dist.full))^2, w = dist.full)
        sig <- sqrt(wmean2 - wmean^2)

        temp <- dnorm(0:max(games$total), mean = wmean, sd = sig)

        norm <- data.frame(Val = temp, row.names = 0:max(games$total))

        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Norm = as.numeric(norm$Val))
        rownames(data_freq) <- 0:max(games$total)

        ggplot(data_freq, aes(x = rownames(data_freq), y = Norm, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(x = "Goals", y = "Probability") +
          scale_color_discrete(name = "", labels = c("Norm", "Mean"))
      }
    } else{
      stop("Can't use TEAM argument, when in.match = TRUE")
    }
  }
  else{
    if(is.null(TEAM)){

      if(is.null(distribution)){
        dist <- table(c(games$awaygoal, games$homegoal))

        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)

        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)

        ggplot(data_freq, aes(x = rownames(data_freq), y = Prob)) +
          geom_histogram(stat = "identity", fill = "blue") +
          labs(title = "Goals scored per Team in each Match", x = "Goals", y = "Probability")
      } else if(isTRUE(distribution == "Normal")){
        dist <- table(c(games$awaygoal, games$homegoal))

        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)

        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        wmean2 <- weighted.mean(as.numeric(names(dist.full))^2, w = dist.full)
        sig <- sqrt(wmean2 - wmean^2)

        temp <- dnorm(0:max(games$awaygoal, games$homegoal), mean = wmean, sd = sig)

        norm <- data.frame(Val = temp, row.names = 0:max(games$awaygoal, games$homegoal))

        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Norm = as.numeric(norm$Val))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)

        ggplot(data_freq, aes(x = rownames(data_freq), y = Norm, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(title = "Density of Goals scored per Team in each Match", x = "Goals", y = "Probability") +
          scale_color_discrete(name = "", labels = c("Norm", "Mean"))
      }
      else if(isTRUE(distribution == "Poisson")){
        dist <- table(c(games$awaygoal, games$homegoal))

        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)

        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        temp <- dpois(0:max(games$awaygoal, games$homegoal), lambda = wmean)

        poi <- data.frame(Val = temp, row.names = 0:max(games$awaygoal, games$homegoal))

        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Poi = as.numeric(poi$Val))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)
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
        dist <- table(c(games$homegoal[games$Team.1 == TEAM], games$awaygoal[games$Team.2 == TEAM]))
        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        temp <- dpois(0:max(games$awaygoal, games$homegoal), lambda = wmean)

        poi <- data.frame(Val = temp, row.names = 0:max(games$awaygoal, games$homegoal))

        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Poi = as.numeric(poi$Val))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)
        ggplot(data_freq, aes(x = rownames(data_freq), y = Poi, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(x = "Goals", y = "Probability") +
          scale_color_discrete(name = "", labels = c("Poi", "Mean"))

      } else if(isTRUE(distribution == "Normal")){
        dist <- table(c(games$homegoal[games$Team.1 == TEAM], games$awaygoal[games$Team.2 == TEAM]))
        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)

        wmean <- weighted.mean(as.numeric(names(dist.full)), w = dist.full)

        wmean2 <- weighted.mean(as.numeric(names(dist.full))^2, w = dist.full)
        sig <- sqrt(wmean2 - wmean^2)

        temp <- dnorm(0:max(games$awaygoal, games$homegoal), mean = wmean, sd = sig)

        norm <- data.frame(Val = temp, row.names = 0:max(games$awaygoal, games$homegoal))

        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)),
                                Norm = as.numeric(norm$Val))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)

        ggplot(data_freq, aes(x = rownames(data_freq), y = Norm, group = 1, color = "red"), size = 1) +
          geom_line() +
          geom_line(aes(y = Prob, color = "blue"), size = 1) +
          theme_bw() +
          labs(title = "Density of Goals scored per Team in each Match", x = "Goals", y = "Probability") +
          scale_color_discrete(name = "", labels = c("Norm", "Mean"))
      }
      else{
        dist <- table(c(games$homegoal[games$Team.1 == TEAM], games$awaygoal[games$Team.2 == TEAM]))
        dist.full <- rep(0, max(games$awaygoal, games$homegoal) + 1)
        names(dist.full) <- 0:max(games$awaygoal, games$homegoal)
        dist.full[names(dist)] <- dist
        cumsum_dist <- cumsum(dist.full)
        data_freq <- data.frame(Freq = as.numeric(dist.full),
                                Prob = as.numeric(dist.full / max(cumsum_dist)),
                                FreqCS = as.numeric(cumsum_dist),
                                ProbCS = as.numeric(cumsum_dist /  max(cumsum_dist)))
        rownames(data_freq) <- 0:max(games$awaygoal, games$homegoal)
        ggplot(data_freq, aes(x = rownames(data_freq), y = Prob)) +
          geom_histogram(stat = "identity", fill = "blue") +
          labs(title = paste("Distribution of Goals:", TEAM, sep = " "), x = "Goals", y = "Probability")
      }
    }
  }
}
allteams <- function(games){
  print(unique(games$Team.1))
}
GermanBundesliga <- read.csv("de.1.csv", encoding = "UTF-8")
PremierLeague <- read.csv("en.1.csv", encoding = "UTF-8")
