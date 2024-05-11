library(tidyverse)
library(gganimate)
library(ggforce)

# Import data in
games <- read.csv("data/games.csv") # Has home/visiting teams and scores
players <- read.csv("data/players.csv") # Has player position (not coords)
plays <- read.csv("data/plays.csv") # Has ballCarrierId
tackles <- read.csv("data/tackles.csv") # Has official recorded tacklers
week1 <- read.csv("data/tracking_week_1.csv") # Player tracking data

# Function that narrows down the data to only grab plays where "tackle" is 
# recorded as an event. Also adds doi_ind
getPlays <- function(week_data) {
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

# Used for plotting later
week1Filtered <- getPlays(week1)
 
# Function that evaluates zones of contact for a given radius
evalContactZones <- function(week_df, radius, play_df = plays, 
                             tackle_df = tackles){

  filteredPlays <- getPlays(week_df)
  
  # Create the id column for play_df and tackle_df
  play_df$gamePlayId <- paste(play_df$gameId, play_df$playId, sep = "-")
  tackle_df$gamePlayId <- paste(tackle_df$gameId, tackle_df$playId, sep = "-")
  
  # Filter down the data to only contain the relevant columns
  weekDf <- filteredPlays[,c(18, 3:5, 10, 11, 17, 19)]
  
  # This adds ballCarrierId to our week data 
  weekDf <- left_join(weekDf, play_df[,c(36, 3)], by = "gamePlayId")
  
  # For every gamePlayId, grab the ball-carrier's coordinates at every frame
  bcCoords <- weekDf %>% 
    filter(nflId == ballCarrierId) %>% 
    select(gamePlayId, frameId, x, y)
  
  # Get the frame the tackle was recorded to have occurred on for each play
  tackleFrames <- weekDf %>% 
    filter(event == "tackle") %>% 
    group_by(gamePlayId) %>% 
    slice(1) %>% # We only need the first row to get the correct frameId
    select(gamePlayId, frameId)
  
  # Initialize a results dataframe to be returned at the end
  results <- list()
  results$radius <- radius
  
  # Now create a week df that only has the DOIs and add the ball carrier coords
  # as well as an indicator if the BC is in a DOI's zone
  defInfo <- weekDf %>% 
    filter(doi_ind == 1) %>% 
    left_join(bcCoords, by = c("gamePlayId", "frameId"), 
              suffix = c(".doi", ".bc")) %>% 
    group_by(frameId) %>% 
    mutate(distToBC = sqrt((x.doi - x.bc)^2 + (y.doi - y.bc)^2),
           bcInZone = ifelse(distToBC < radius, 1, 0))
  
  # One issue that arises is when we have pass plays, sometimes the BC is 
  # already in a zone before they even have the ball, so it acts like there are
  # being tackled even before the ball gets to them. It may be more accurate to
  # only count the zone as being active after the ball arrives, as that is when
  # the tackle can legally begin. ALSO, BIG POINT: the BC may just be running a
  # route and passing through zones before they actually get the ball, and those
  # are being counted. We only want to count once they are officially a BC.
  ballArrivals <- weekDf %>% 
    filter(event %in% c("pass_arrived", "pass_outcome_caught")) %>%
    group_by(gamePlayId) %>%
    summarize(ballArrivalFrame = min(frameId))
  
  defInfo <- defInfo %>% 
    left_join(ballArrivals, by = "gamePlayId") %>%
    # If pass play, turn all bcInZone values before the pass arrives to 0
    mutate(bcInZone = ifelse(!is.na(ballArrivalFrame) & 
                               frameId < ballArrivalFrame, 0, bcInZone))
  
  # Add the frame first_contact first appears, used to compare with first zone
  # entry
  fcFirstFrames <- defInfo %>% 
    filter(event == "first_contact") %>% 
    group_by(gamePlayId) %>% 
    summarize(fcFrame = min(frameId))
  
  # Count how many zones the BC enters in each play
  zoneEntries <- defInfo %>%
    left_join(tackleFrames, by = "gamePlayId") %>% 
    left_join(fcFirstFrames, by = "gamePlayId") %>% 
    # Only grab times where the BC was in the zone BEFORE the event "tackle"
    filter(bcInZone == 1, frameId.x < frameId.y) %>% 
    group_by(gamePlayId) %>%
    summarize(numZonesEntered = length(unique(nflId)),
              tacklerIds = list(unique(nflId)),
              firstEntryDiff = min(fcFrame) - min(frameId.x)) %>% 
    filter(gamePlayId != "2022091102-1517") # No tackle recorded for this play
  
  # summary(zoneEntries$numZonesEntered)
  # summary(zoneEntries$firstEntryDiff)
  # hist(na.omit(zoneEntries$firstEntryDiff))
  # boxplot(na.omit(zoneEntries$firstEntryDiff), horizontal = T)
  results$meanEntryDiff <- mean(zoneEntries$firstEntryDiff, na.rm = T)
  results$medianEntryDiff <- median(zoneEntries$firstEntryDiff, na.rm = T)
  
  # Now we can compare these numbers to the numbers of DOIs credited on a tackle
  filteredTackles <- tackle_df[tackle_df$gamePlayId%in%zoneEntries$gamePlayId,]
  
  officialTacklers <- filteredTackles %>% 
    group_by(gamePlayId) %>% 
    summarize(numTacklers = length(unique(nflId)),
              officialTacklerIds = list(unique(nflId)))
  #summary(officialTacklers$numTacklers)
  
  # Merge these two results so that we can directly compare better
  #compareDf <- merge(zoneEntries, officialTacklers, by = "gamePlayId")
  # zoneEntries <- zoneEntries %>% 
  #   mutate(Category = 'Zones Entered', Value = numZonesEntered) %>% 
  #   select(Category, Value)
  # tacklePlayerCounts <- officialTacklers %>% 
  #   mutate(Category = 'Actual Tackler Count', Value = numTacklers) %>% 
  #   select(Category, Value)
  # compareDf <- bind_rows(zoneEntries, officialTacklers)
  # 
  # ggplot(compareDf, aes(x = as.factor(Value), fill = Category)) +
  #   geom_bar(position = "dodge") +
  #   labs(x = element_blank(), 
  #     title = "Actual Credited Tackler Count vs. Claimed Count",
  #     subtitle = "Filtering out where BC entered zone after tackle event was recorded") +
  #   theme_minimal() 
  
  compareIdsDf <- zoneEntries %>% 
    left_join(officialTacklers, by = "gamePlayId") %>% 
    mutate(
      # Boolean column that is 1 if all predicted are accurate, 0 if not
      correct = map2_int(tacklerIds, officialTacklerIds, ~ setequal(.x, .y)),
      # Count of players we missed (get rid of length for actual IDs)
      missingIds = map2_int(officialTacklerIds, tacklerIds, 
                            ~ length(setdiff(.x, .y))),
      # Count of players we accidentally counted (remove length for actual IDs)
      extraIds = map2_int(tacklerIds, officialTacklerIds, 
                          ~ length(setdiff(.x, .y)))
    )
  
  # mean(compareIdsDf$numZonesEntered > compareIdsDf$numTacklers)
  # mean(compareIdsDf$numZonesEntered < compareIdsDf$numTacklers)
  # mean(compareIdsDf$numZonesEntered == compareIdsDf$numTacklers)
  
  # What % do we match up exactly?
  results$correctPct <- mean(as.integer(compareIdsDf$correct)) # 39.38% for smaller radius
  
  # How many are we missing?
  #summary(compareIdsDf$missingIds)
  #barplot(table(compareIdsDf$missingIds))
  results$noneMissingPct <- mean(compareIdsDf$missingIds == 0) # 73% where we aren't missing any
  
  # How many are we adding?
  #summary(compareIdsDf$extraIds)
  #barplot(table(compareIdsDf$extraIds))
  # Complement would be the % all of our predictions were right (but could still
  # be missing players)
  results$extraPct <- mean(compareIdsDf$extraIds != 0) # Adding at least one extra ~68% of the time
  
  # Returning defInfo for plotting purposes
  return(list(results = data.frame(results), defInfo = defInfo))
}

radii <- seq(0.9, 2.2, by = 0.05)

# Initialize results df
results <- data.frame(radius = numeric(), meanEntryDiff = numeric(), 
                      medianEntryDiff = numeric(), correctPct = numeric(), 
                      noneMissingPct = numeric(), extraPct = numeric())

for (rad in radii[1:2]) {
  # Get results for current radius and then rbind them to results df
  tempRes <- evalContactZones(week1, radius = rad)$results
  results <- rbind(results, tempRes)
}

# Plot mean and median entry diffs (mean and median number of frames between the
# BC first entering the zone and the first_contact label)
plot(meanEntryDiff ~ radius, results, col = "red", type = "b", pch = 16, lty =2,
     xlab = "Radius", ylab = "Frame Difference", 
     main = "Mean and Median Entry Frame Differences")
lines(medianEntryDiff ~ radius, results, col = "black", type = "b", pch = 16)
legend("bottomright", c("Mean", "Median"), col = c("red", "black"), 
       lty = c(2, 1), pch = 16)
abline(v = 1.2, col = "blue", lty = 3)

# Plot noneMissingPct across radii (% of games where we didn't miss any of the
# actual tacklers)
plot(noneMissingPct ~ radius, results, type = "b", pch = 16,
     xlab = "Radius", ylab = "Proportion of No Missing Players", 
     main = "None Missing % For Different Radii")
abline(v = results$radius[which.max(results$noneMissingPct)], 
       col = "red", lty = 2)

plotGame <- function(gamePlayId, radius, week_df, animate = T) {
  # week_df is filtered week data from getPlays()
  
  # Get the data for every player (not just DOIs) from the week data
  play <- week_df[week_df$gamePlayId == gamePlayId,]
  #play <- week_df[(week_df$gamePlayId == gamePlayId) & (week_df$frameId == 1),]

  # Get defInfo for plotting the actual zones
  defInfo <- evalContactZones(play, radius)$defInfo
  defInfo <- defInfo[defInfo$gamePlayId == gamePlayId,]
  #defInfo <- defInfo[(defInfo$gamePlayId == gamePlayId)&(defInfo$frameId==1),]
  
  p <- ggplot() +
    geom_circle(data = defInfo, aes(x0 = x.doi, y0 = y.doi, r = radius, 
                                    fill = as.factor(bcInZone), group = nflId), 
                col = NA, alpha = 1) +
    scale_fill_manual(values = c("0" = "gray", "1" = "green"), guide = "none")+
    # Everybody but the football gets a label
    geom_text(data = subset(play, club != "football"),
              aes(x = x, y = y, col = club, label = jerseyNumber),
              fontface = "bold") +
    scale_color_manual(values = c("red", "blue")) +
    # Add a brown dot to act as the football
    geom_point(data = subset(play, club == "football"), 
               aes(x = x, y = y), fill = "darkorange4", shape = 21, 
               size = 3) +
    coord_fixed() +
    theme_minimal() +
    theme(panel.background = element_rect(fill = "darkseagreen3")) +
    guides(col = guide_legend(title = "Team")) +
    labs(title = paste0("Game ", gamePlayId))
  
  if (animate) {
    p <- p + transition_time(frameId) +
      labs(title = paste0("Game ", gamePlayId),
           subtitle = "Frame: {frame_time}")
    return(animate(p, nframes = length(play$frameId) / 23, fps = 10))
  } else {
    return(p)
  }
}

#sample(unique(week1Filtered$gamePlayId), 1) # Get a random game in the data
plotGame("2022091100-1260", 1.2, week1Filtered, animate = T)