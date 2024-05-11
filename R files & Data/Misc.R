
# This code allows us to plot and compare the counts of actual tacklers on a
# play vs the claimed count (e.g. how many zones did BC enter)
bcInZoneSums <- bcInZoneSums %>% 
  mutate(Category = 'Zones Entered', Value = numZonesEntered) %>% 
  select(Category, Value)
tacklePlayerCounts <- tacklePlayerCounts %>% 
  mutate(Category = 'Actual Tackler Count', Value = numTacklers) %>% 
  select(Category, Value)
compareDf <- bind_rows(bcInZoneSums, tacklePlayerCounts)

ggplot(compareDf, aes(x = as.factor(Value), fill = Category)) +
  geom_bar(position = "dodge") +
  labs(x = element_blank(), 
       title = "Actual Credited Tackler Count vs. Claimed Count, No Filtering")+
  theme_minimal() 

# Find average length of time between each first contact and the tackle
eventTimes <- week1Filtered %>%
  filter(event %in% c("first_contact", "tackle")) %>%
  group_by(gamePlayId, event) %>%
  summarize(frame = min(frameId)) %>% 
  # This separates event into two columns with values for frame
  spread(key = event, value = frame) %>% 
  mutate(diff = tackle - first_contact)

# Some games have NAs for first_contact
sum(is.na(eventTimes$first_contact))
mean(is.na(eventTimes$first_contact))

eventTimes <- na.omit(eventTimes)
summary(eventTimes$diff)
hist(eventTimes$diff)
boxplot(eventTimes$diff, horizontal = T)
