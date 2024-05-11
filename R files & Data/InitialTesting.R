library(tidyverse)
library(gganimate)
library(ggforce)
# Import data in
games <- read.csv("data/games.csv") # Has home/visiting teams and scores
players <- read.csv("data/players.csv") # Has player position (not coords)
plays <- read.csv("data/plays.csv")
tackles <- read.csv("data/tackles.csv")
week1 <- read.csv("data/tracking_week_1.csv")

# Might need dummy variable for if defense is starting on their side or not
# Might just be able to use absolute yard number?

# # https://nfl-video.com/buffalo-bills-vs-los-angeles-rams-full-game-replay-2022-nfl-week-1
# temp <- week1[week1$playId == 56 & week1$gameId == 2022090800 & week1$nflId %in% c(42489, 43294, NA),]
# tempTackleInfo <- tackles[tackles$playId == 56 & tackles$gameId == 2022090800,]
# tempPlayInfo <- plays[plays$playId == 56 & plays$gameId == 2022090800,]
# 
# temp <- week1[week1$playId == 80 & week1$gameId == 2022090800 & week1$nflId %in% c(46076, 53532, NA),]
# tempTackleInfo <- tackles[tackles$playId == 80 & tackles$gameId == 2022090800,]
# tempPlayInfo <- plays[plays$playId == 80 & plays$gameId == 2022090800,]

getPlays <- function(week_data) {
  # Function that narrows down the data to only grab plays where "tackle" is 
  # recorded as an event
  # Get every frame with tackle or qb_sack recorded
  tackleFrames <- week_data[week_data$event %in% c("tackle", "qb_sack"),]
  # Get unique game and play codes for each of these
  gamePlayCodes <- unique(tackleFrames[1:2])
  # Combine the two codes to make a single indicator
  tackleGamePlayInds <- paste(gamePlayCodes$gameId, gamePlayCodes$playId, 
                              sep = "-")
  
  # Now create this same indicator but for every row in the weekly data
  week_data$gamePlayId <- paste(week_data$gameId, week_data$playId, sep = "-")
  
  # Now only grab the plays in the weekly data that have matching gamePlayInds
  filteredPlays <- week_data[week_data$gamePlayId %in% tackleGamePlayInds,]
  
  # Give the football the label "B" for ball
  # filteredPlays[filteredPlays$displayName == "football", "jerseyNumber"] <- "B"
  
  # Get all the defenders of interest (DBs and LBs, everyone but the d-line)
  defOfInterest <- players[players$position %in% c("CB", "DB", "FS", "ILB", "MLB",
                                                   "OLB", "SS"), c(1, 6, 7)]
  # Add indicator for if player is a defender of interest or not
  filteredPlays$doi_ind <- ifelse(filteredPlays$nflId%in%defOfInterest$nflId,1,0)
  
  return(filteredPlays)
}

week1Filtered <- getPlays(week1)



# Avg wingspan is roughly 76 inches (2.11 yards) for d-backs
# https://steelersnow.com/official-measurements-for-2022-nfl-combine-defensive-backs/
# Avg wingspan for LBs is roughly 76.93 inches (2.14 yards) for LBs
# https://steelersnow.com/official-measurements-for-2022-nfl-combine-linebackers/

# Plot a frame again but now have "zones of contact" for each d-back
# Each zone should be a circle with radius 2.11 yards (avg wingspan) 
# (also maybe look into making it a cone from wherever they are facing)
# Choosing smaller radius because I don't think people can tackle with their
# fingertips. Can be adjusted later. Could also make sep inds for DBs and LBs
# UPDATE: adjusted in ZoneOfContact.R

gameId <- 2022091102
playId <- 2373
temp <- week1Filtered[week1Filtered$playId == playId & 
                      week1Filtered$gameId == gameId,]  
                      #& week1Filtered$frameId == 30,]
tempTackleInfo <- tackles[tackles$playId == playId & tackles$gameId == gameId,]
tempPlayInfo <- plays[plays$playId == playId & plays$gameId == gameId,]

# Create dataframe made up of only the ball carrier's data
bcInfo <- temp[temp$nflId == tempPlayInfo$ballCarrierId & !is.na(temp$nflId),]
               #& temp$frameId == 30, ]

# Create dataframe with info for each of the defenders
# Information includes distances from ball-carrier and if the ball-carrier is in 
# their hypothesized zone of contact
defInfo <- temp %>% 
  filter(doi_ind == 1) %>% 
  group_by(frameId) %>% 
  mutate(
    # Euclidean distance from ball-carrier 
    distToBC = sqrt((x - bcInfo$x[frameId])^2 + (y - bcInfo$y[frameId])^2),
    #distFromBC = sqrt((x - bcInfo$x)^2 + (y - bcInfo$y)^2),
    bcInZone = as.factor(ifelse(distToBC < 1.7786, 1, 0))
  )

# Plot the play
# p <- ggplot(temp, aes(x = x, y = y, col = club, label = jerseyNumber)) +
#   geom_circle(data = defInfo, aes(x0 = x, y0 = y, r = 1.7786, 
#                                   fill = as.factor(bcInZone), group = nflId), 
#               inherit.aes = F, col = NA, alpha = 0.3) +
#   scale_fill_manual(values = c("0" = "gray", "1" = "green3")) +
#   geom_text() +
#   coord_fixed() +
#   theme_minimal()
# p
ggplot(temp) +
  geom_circle(data = defInfo, aes(x0 = x, y0 = y, r = 1.25, 
                                  fill = as.factor(bcInZone), group = nflId), 
              col = NA, alpha = 0.3) +
  scale_fill_manual(values = c("0" = "gray", "1" = "green3")) +
  # Everybody but the football gets a label
  geom_text(data = subset(temp, club != "football"),
            aes(x = x, y = y, col = club, label = jerseyNumber)) +
  # Add a brown dot to act as the football
  geom_point(data = subset(temp, club == "football"), 
             aes(x = x, y = y), fill = "darkorange4", shape = 21, 
             size = 3) +
  coord_fixed() +
  theme_minimal() 


# Animated plot
p + transition_time(frameId) +
  labs(title = "Frame: {frame_time}")

# p + transition_manual(frameId) +
#   labs(title = "Frame: {current_frame}")
