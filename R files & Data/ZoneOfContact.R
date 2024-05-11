# ESTIMATING ZONE OF CONTACT FOR SECONDARY/LINEBACKERS
library(tidyverse)
# Start by getting necessary data in
#games <- read.csv("data/games.csv") # Has home/visiting teams and scores
players <- read.csv("data/players.csv")
plays <- read.csv("data/plays.csv")
tackles <- read.csv("data/tackles.csv")
week1 <- read.csv("data/tracking_week_1.csv")

# Get all the defenders of interest (DBs and LBs, everyone but the d-line)
defOfInterest <- players[players$position %in% c("CB", "DB", "FS", "ILB", "MLB",
                                                 "OLB", "SS"), c(1, 6, 7)]

# This function takes a week of data and narrows it down to only the plays where
# the event "first_contact" is recorded. The idea is, if we can figure out how
# far away the closest player was when first_contact was recorded for each of
# these plays, we can figure out a better estimate for how wide the "zone of
# contact" for the players needs to be. This also only grabs plays where a 
# defender of interest (not the d-line) made the tackle. It is possible that a
# d-lineman made first contact but did not successfully tackle but that is not
# common. It does pose issues but those are addressed later.

# week_data = week1
# tackle_data = tackles
# defenders = defOfInterest
# play_data = plays


getContactPlays <- function(week_data, tackle_data = tackles, 
                            defenders = defOfInterest, play_data = plays) {
  
  contactFrames <- week_data[week_data$event %in% c("first_contact"),]
  
  # Get unique game and play codes for each of these
  gamePlayCodes <- unique(contactFrames[1:2])
  # Combine the two codes to make a single indicator
  contactGamePlayInds <- paste(gamePlayCodes$gameId, gamePlayCodes$playId, 
                               sep = "-")
  # Now create this same indicator but for every row in the weekly & tackle data
  week_data$gamePlayId <- paste(week_data$gameId, week_data$playId, sep = "-")
  tackle_data$gamePlayId <- paste(tackle_data$gameId, tackle_data$playId, 
                                  sep = "-")
  play_data$gamePlayId <- paste(play_data$gameId, play_data$playId, sep = "-")
  
  # Get only play inds where the tackle was recorded by a defender of interest
  # and are in the timeframe we are analyzing 
  filteredTackles <- tackle_data[(tackle_data$gamePlayId %in% 
                                 contactGamePlayInds) &
                                 (tackle_data$nflId %in% defenders$nflId),]
  
  # And now get only the plays where a filteredTackle happened, and only the
  # frame where first contact was made
  filteredPlays <- week_data[(week_data$gamePlayId %in% 
                             filteredTackles$gamePlayId) &
                             (week_data$event == "first_contact") &
                             !is.na(week_data$event),]
  
  # Get ball-carriers for every play
  BCs <- play_data[play_data$gamePlayId %in% filteredPlays$gamePlayId, 1:4]
  
  # Merge filteredPlays and BCs so that we can create a dummy variable for if a
  # player was the ball-carrier in a play or not
  merged <- left_join(filteredPlays, BCs[-4], by = c("gameId", "playId"))
  
  # Add dummy variables for if the player is a defender of interest or the BC
  merged <- merged %>% 
    mutate(doi_ind = ifelse(nflId %in% defenders$nflId, 1, 0),
           bc_ind = ifelse(nflId == ballCarrierId, 1, 0)) %>% 
    select(!ballCarrierId)
  
  # Now get a new df with info about each defender of interest. Namely, we're
  # interested in distance from the ball-carrier on their respective plays
  
  # First we need info on the ball-carriers, mainly their x/y coords
  bcInfo <- merged %>% 
    filter(bc_ind == 1) %>% 
    select(gameId, playId, bc_x = x, bc_y = y)
  
  # Now join these coords to just the defenders of interest so we can calculate
  # the distance from each to the BC
  defInfo <- merged %>% 
    filter(doi_ind == 1) %>% 
    left_join(bcInfo, by = c("gameId", "playId")) %>% 
    mutate(distToBC = sqrt((x - bc_x)^2 + (y - bc_y)^2)) %>% 
    select(gameId, playId, nflId, displayName, jerseyNumber, distToBC)
  
  return(list(defInfo = defInfo, filteredTackles = filteredTackles))
}

gcp <- getContactPlays(week1)
defInfo <- gcp$defInfo
defInfo$gamePlayId <- paste(defInfo$gameId, defInfo$playId, sep = "-")

# Get the closest distances to the BC for each play
minDists <- defInfo %>% 
  group_by(gamePlayId) %>%
  mutate(minIdx = which.min(distToBC)) %>% 
  summarize(nflId = first(nflId[minIdx]),
            minDist = min(distToBC))

summary(minDists$minDist) # 14 yards is really high...
# 1:24:40 for play
# https://nfl-video.com/indianapolis-colts-vs-houston-texans-full-game-replay-2022-nfl-week-1

boxplot(minDists$minDist, horizontal = T,
        xlab = "Min. Dist from a DOI to the BC at first contact (in yds)")
# A lot of the outliers are just too far to be possible. What realistically
# happened here is that one of the linemen made first contact, while all the 
# DOIs were further back. Even though a DOI ended up finishing the play, the
# BC was touched earlier by a lineman. 
# We need to also include d-linemen if we want to correctly identify who
# made first contact. After assigning first contact to a player, we could then 
# filter to only grab first contact by a DOI. Would be more consistent than
# below, but more time-consuming for how much gain?

#head(sort(minDists$dist, decreasing = T), 50)

# After experimenting and watching lots of plays, I've determined that cutting
# off any plays where the min distance was > 2.5 yds seems reasonable. We
# probably are losing a couple of plays where the DOI did make first contact,
# but it's cutting off a lot more of the d-line situations. Also, the outliers
# leftover seem to be legitimate DOI first contacts, their big distance could be
# explained due to error in the chip readings or more likely human labeling 
# errors

outlierIds <- minDists[minDists$minDist > 2.5, "gamePlayId"] # Filter for later
minDists <- minDists[!(minDists$minDist > 2.5),]
boxplot(minDists$minDist, horizontal = T)
summary(minDists$minDist)
hist(minDists$minDist)
sd(minDists$minDist)
mean(minDists$minDist)+3*sd(minDists$minDist) # 2.293838
mean(minDists$minDist)+2*sd(minDists$minDist) # 1.86241
mean(minDists$minDist)+sd(minDists$minDist) # 1.430982

# REMEMBER: goal is to get a good estimate for the radius of the zone of contact

# Problem with using 2.5: even though pretty much everything is an actual 
# contact, it's just too big. Yes it captures some outliers but a lot of tackles
# will be counted as starting way sooner than they should. Realistically we
# need something smaller; we will lose out on some of the very first frames of
# contact but will probably be more accurate overall.

#### BETTER WAY OF EVALUATING RADIUS #### 

# This will help us figure out how many times the closest defender at the
# first_contact event ends up being an actual tackler:

# Get only tackles made by DOIs (created in getContactPlays)
filteredTackles <- gcp$filteredTackles
# Filter out the outlier games (where minDist was > 2.5)
filteredTackles <- filteredTackles[!(filteredTackles$gamePlayId %in% 
                                       outlierIds$gamePlayId),]
length(unique(filteredTackles$gamePlayId)) # 1034 tackles in total
# Left join this with our minDist df. Any tackle that was identified to the 
# wrong player will have an NA for minDist
tackleIdAcc <- left_join(filteredTackles, minDists, by=c("gamePlayId", "nflId"))

summary(tackleIdAcc[,4:7])
# A play can be labelled as both fumble and tackle/assist.
# There are 6 plays that are labeled as missed tackle and either tackle/assist.
# Not sure how that works.

# IMPORTANT NOTE: A lot of plays credit multiple defenders on a tackle. minDists
# only identifies one player as the tackler per play. This means that any play
# with multiple tacklers will have at least one NA, which we do not want to 
# count. I only want to count games from filteredTackles that have only NAs, as
# that means we did not correctly identify even one tackler. That's what this
# code below does.
missedPlays <- tackleIdAcc %>%
  group_by(gamePlayId) %>%                
  filter(all(is.na(minDist))) %>%  # Keep groups where ALL minDists are NA
  select(gamePlayId, gameId, playId)
length(unique(missedPlays$gamePlayId)) # 128 missed tacklers 
# 128/1034 = 0.1238 = prop. of misidentified tacklers

# So...what does this mean exactly? Remember, we've filtered the data so we only
# have plays where the event first_contact was labelled. The goal is to estimate
# how big the zone of contact should be, so I'm making a few assumptions:
#    1. The first_contact event label is true, as in that is truly the frame
#       where first contact was made.
#    2. When first_contact is recorded, the player closest to the ball-carrier
#       is the one making the contact.
# There's also a smaller assumption that since we filtered the data to also only
# include plays where the final tackle was made by a DOI, that there are not any
# non-DOIs closer to the ball-carrier that may be making contact first. This is
# not true, we just want it to hold for the most part. Assumption 1 is likely 
# not true as the labels are made by humans, who are not perfect. I'd like to
# say the humans are probably right most of the time though, as generally it is
# clear when two players come into contact in a play. Assumption 2 is not
# necessarily true either, but I think it holds in most cases.
#
# The code above tested some of these assumptions. If most of the tacklers are
# being identified correctly, then Assumption 2 may hold which means that those
# minDists are potentially good values for the radius of our zone of contact. 
# From the results above, it looks like working under these assumptions results
# in identifying a tackler correctly 87.62% time. The other 12.38% are the
# results of one of two cases: either a non-DOI was closer and was the one to
# actually make first contact, or the DOI did make first_contact but failed to
# make a tackle. I am fine with the latter case being misclassified, as the goal
# is to identify when contact happens; it doesn't matter if a tackle is made or 
# not. Unfortunately, I think it is more likely that a non-DOI made contact
# first, and in that case the minDist is not a reliable radius for zone of 
# contact since the DOI was not actually making contact.

missedGameIds <- minDists$gamePlayId %in% missedPlays$gamePlayId
par(mfrow = c(1, 2))
hist(minDists[!missedGameIds,]$minDist, freq = F, breaks = 15,
     main = "Minimum Distances for Correctly Identified Games", 
     xlab = "Min. Distance (in yds)")
hist(minDists[missedGameIds,]$minDist, freq = F, breaks = 15,
     main = "Minimum Distances for Misidentified Games", 
     xlab = "Min. Distance (in yds)")
# summary(minDists[!missedGameIds,]$minDist)
# summary(minDists[missedGameIds,]$minDist)

# The histogram for the games where we correctly identified a tackler is
# positively skewed, which is what I would have guessed it to be. The closer you
# are, the more likely it is you are to make contact. Also, there is an upper
# limit; as you get further and further away from the BC, the less possible it
# becomes to actually make contact and begin a tackle. There are physical
# limitations on how far a person can reach, so we wouldn't expect to see as 
# many large distances. 
#
# When we look at the histogram for the games where we did not correctly 
# identify the tackler, you can see that it has shifted to the right, and looks
# closer to being normally distributed than those we got correct. That means
# that those games tend to have higher minimum distances. As stated before, the
# higher the minimum distance, the less likely it is to even be physically
# possible to make contact with another player. The fact that these more 
# unlikely scenarios show up more when we get the player wrong indicates to me
# that what is probably happening is that there is a non-DOI closer to the BC 
# that made contact first, and that is throwing our results off. Not every
# distance is large, there are still some smaller ones that could potentially be
# attributed to a DOI missing the tackle, among other explanations.
#
# The big question is, is it an issue that the minimum distances identify the
# incorrect player 12.38% of the time? I mean ideally it would be less, but it
# seems like we're correct enough of the time that using values for the zone of
# contact based on these minimum distances wouldn't be an issue. Even when we're
# wrong, we're not off by that much. Personally, I think that is an acceptable
# amount of error. To estimate some potential values for the radius of the zone,
# I will look at values solely from the distances from the games where we
# identified tacklers correctly, which should hopefully reduce some of the
# potential error. 

correctGames <- minDists[!missedGameIds,]
par(mfrow=c(1,1))
hist(correctGames$minDist)
summary(correctGames$minDist)
sd(correctGames$minDist)
mean(correctGames$minDist) + sd(correctGames$minDist) # 1.371767
#ecdf(correctGames$minDist)(1.371767)
mean(correctGames$minDist) + 2*sd(correctGames$minDist) # 1.778603
#ecdf(correctGames$minDist)(1.778603)
mean(correctGames$minDist) + 3*sd(correctGames$minDist) # 2.185438
#ecdf(correctGames$minDist)(2.185438)

#### ZONE EVALUATION ####
# Everything above was just to try to determine a number to test as the radius
# of our zone, based of the labelled first_contact events, as those are the only
# points we have in the data where we can be relatively sure that some sort of
# player contact has begun. Once we decide on a value, we need to test it on
# entire plays so we can see if it is too big or too small. Namely, here are
# some things I think are important to look out for:
#     * Out of all the players the tackles df lists as being a part of a tackle,
#       how many of them are correctly identified as having the BC in their
#       zone? (This may not be very helpful as I think it's highly likely
#       they'll always all end up on the BC at the end)
#     * How many DOIs in total are said to have had the BC in their zone at one
#       point or another? (If rather high, could indicate that the circles are
#       maybe too big, and vice versa)
#     * Maybe how long the BC is in a zone compared to length of time between
#       first_contact and tackle? (Problem: we have confirmed first_contact is
#       not completely reliable, so for plays where a DOI did not actually make)
#     * Entry before first contact: potential way to see if zone is too big. Are
#       BCs entering a zone well before first_contact is recorded? First_contact
#       has error but shouldn't be off my more than a couple frames.
#     * Potentially interesting to look at and map: density of zone entries,
#       where on the field are BCs most often entering zones of contact?

# This function gets a count of how many defenders are said to have come into
# contact with the ball-carrier within the course of a play, along with their
# IDs. These can be compared to the tackle df that tells us who all was 
# credited on a tackle. The closer our numbers line up, the more accurate the
# circle's radius is.
evalContactZones <- function(week_df, radius, play_df = plays, 
                             tackle_df = tackles) {
  # week_df needs to be a transformed week df passed through getPlays()
  
  # Create the id column for play_df and tackle_df
  play_df$gamePlayId <- paste(play_df$gameId, play_df$playId, sep = "-")
  tackle_df$gamePlayId <- paste(tackle_df$gameId, tackle_df$playId, sep = "-")
  
  # Filter down the data to only contain the relevant columns
  weekDf <- week_df[,c(18, 3:5, 10, 11, 17, 19)]
  
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
  
  # Now create a week df that only has the DOIs and add the ball carrier coords
  # as well as an indicator if the BC is in a DOI's zone
  #radius = 1.371767
  #radius = 1.778603
  
  results$radius <- radius
  
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
  ballArrivals <- week_df %>% 
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

res1 <- evalContactZones(week1Filtered, radius = 1.371767)$results
res2 <- evalContactZones(week1Filtered, radius = 1.778603)$results
res3 <- evalContactZones(week1Filtered, radius = 2.185438)$results

results <- rbind(res1, res2, res3)

#### TESTING RADII ####
# I have three estimates of what a good radius may be, but I think a better way
# to test for and find an optimal value would be to iterate through a bunch of
# potential radii, plotting the statistics from above results for each one. This
# may help us visualize what an ideal radius may look like.

# The median minDist to the BC at first_contact was about 0.9 so starting there.
# Going up to 2.2, which is right above our highest estimated radius 
radii <- seq(0.9, 2.2, by = 0.05)

# Initialize results df
results <- data.frame(radius = numeric(), meanEntryDiff = numeric(), 
                      medianEntryDiff = numeric(), correctPct = numeric(), 
                      noneMissingPct = numeric(), extraPct = numeric())

for (rad in radii) {
  # Get results for current radius and then rbind them to results df
  tempRes <- evalContactZones(week1Filtered, radius = rad)$results
  results <- rbind(results, tempRes)
}

# Plot correctPct across radii (% of games where our players matched exactly
# with the real tacklers)
plot(correctPct ~ radius, results, type = "b", pch = 16,
     xlab = "Radius", ylab = "Proportion of All Correct Players", 
     main = "All Correct % For Different Radii")

# Plot noneMissingPct across radii (% of games where we didn't miss any of the
# actual tacklers)
plot(noneMissingPct ~ radius, results, type = "b", pch = 16,
     xlab = "Radius", ylab = "Proportion of No Missing Players", 
     main = "None Missing % For Different Radii")
abline(v = results$radius[which.max(results$noneMissingPct)], 
       col = "red", lty = 2)

# Plot extraPct across radii (% of games where we added an extra player to the
# tackle)
plot(extraPct ~ radius, results, type = "b", pch = 16,
     xlab = "Radius", ylab = "Proportion of Extra Players Added", 
     main = "Extra Player % For Different Radii")
#abline(v = 1.3, col = "red", lty = 2)

# Plot mean and median entry diffs (mean and median number of frames between the
# BC first entering the zone and the first_contact label)
plot(meanEntryDiff ~ radius, results, col = "red", type = "b", pch = 16, lty =2,
     xlab = "Radius", ylab = "Frame Difference", 
     main = "Mean and Median Entry Frame Differences")
lines(medianEntryDiff ~ radius, results, col = "black", type = "b", pch = 16)
legend("bottomright", c("Mean", "Median"), col = c("red", "black"), 
       lty = c(2, 1), pch = 16)
abline(v = 1.2, col = "blue", lty = 3)

# Results:
#
# When considering entry times:
# 1.2 seems like the best choice, both mean and median are almost 0, meaning it
# most accurately lines up with the first_contact event
# Around 1.35 may not be a bad choice either as both mean and median are very
# very close to each other and to 1, meaning usually it gets them a frame too
# early which isn't necessarily a bad thing
# 1.65 is the highest I would go here, about 2 frames before first_contact
#
# When considering None Missing %:
# 1.25 and 1.3 are almost identical in having the highest non-missing %, meaning
# that at around those levels we getting the most real tacklers accurately.
# 1.55 has a similar peak but a little lower.
#
# When considering correct %:
# I don't think this one is as useful as the previous ones. We can see a pretty
# sharp decline in our accuracy when radius > 1.15, but that is to be expected:
# bigger circles mean more players get counted, and in our data the majority of
# plays are only credited to 1 tackler:
# barplot(table(compareIdsDf$numTacklers))
# so if you have a very small circle, you'll probably only end up identifying
# one tackler which is more often than not correct. The goal isn't to correctly
# identify tacklers though, it's to determine a good starting point for contact
# in a tackle, so this measure does not seem as helpful.
#
# When considering extra %:
# This is also not very helpful, it just states the obvious: the bigger the
# circle, the more extra players we say were involved in the tackle. The inverse
# of this would be the proportion where all of our predicted players would be
# right (though we could still be missing some, all x in y does not mean all y
# in x). I don't put much stock in this one, but it looks like it starts spiking
# significantly for radii > 1.15.
#
# Summarizing:
# Personally, I put the most faith in the mean/median entry times. Everything
# else measures if the BC ended up in the circles of the people listed as 
# officially tackling, which I think still has use but it doesn't directly
# involve contact. Even though I think first_contact probably has a lot of
# issues, it's the closest thing we have to a definitive moment of contact, so
# the stat that looks at our radius and its effect on something related to
# first_contact seems more useful to me. With this in mind, I think 1.2, 1.35, 
# and 1.65 could all be decent choices. 1.2 seems the best as it has values
# around 0, meaning it is almost exactly hitting the first_contact event on
# average. 1.35 and 1.65 would be good if you wanted a little more padding, so
# if you want to be grabbing 1 or 2 frames ahead of first_contact. 1.25 also
# seems like a good choice considering it is close to the largest % of
# non-missing player %s, meaning not only does it time well with first_contact,
# it is not missing many of the officially listed tacklers.

# 1.25 is the 78th percentile (based on the "correct" games from earlier that I
# based my initial guesses off of)
ecdf(correctGames$minDist)(1.2)

#### PLOTTING ####
library(ggforce)
library(gganimate)
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
    #return(animate(p, nframes = length(play$frameId) / 23, fps = 10))
    return(animate(p, nframes = length(play$frameId) / 23, device = "png",
            renderer = file_renderer("~/gganim", prefix = "gganim_plot", overwrite = TRUE)))
  } else {
    return(p)
  }
}
# I used this to sample random games to watch to visually test zones
# sample(unique(week1Filtered$gamePlayId), 1)

plotGame("2022091100-1260", 1.2, week1Filtered, animate = T)

anim_save("gameExample1.gif", path = "C:/Users/Den/OneDrive - Kansas State University/College/STAT 764/Final Project/Images")

play_df[play_df$gamePlayId == "2022091100-1260",]

# Not a bad one to save: 2022091113-246
# Good example of a pass play where defender is on the BC before the catch:
# 2022091113-3455

# Save this one: 2022091100-1260   41:52
#https://nfl-video.com/new-orleans-saints-vs-atlanta-falcons-full-game-replay-2022-nfl-week-1

# "2022091102-2373" game with error, for some reason ball and BC just stop while
# rest of players move forward. Maybe because dogpile messes up tracking?
# 57:40
# https://nfl-video.com/san-francisco-49ers-vs-chicago-bears-full-game-replay-2022-nfl-week-1
