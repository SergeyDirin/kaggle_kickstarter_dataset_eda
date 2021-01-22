
# categories

d_summ <- d %>%
  count(main_category, state, wt = NULL, sort = T)

ggplot(main_cat) +
  geom_bar(aes(x = reorder(main_category, n), y = n), stat = "identity") + 
  xlab("Main Category") + 
  ylab("Count")+
  coord_flip()

ggplot(main_cat) +
  geom_bar(aes(x = reorder(main_category, n), y = n, fill = state), stat = "identity") + 
  xlab("Main Category") + 
  ylab("Count")+
  coord_flip() +
  ggtitle("Most Frequent Categories")

#sub categories
subcats_count <- d %>% 
  group_by(main_category) %>%
  summarise(subCats = length(unique(category)))

ggplot(subcats_count) +
  geom_bar(aes(x = reorder(main_category, subCats), y = subCats), stat = "identity") + 
  xlab("Main Category") + 
  ylab("SubCategory Count")+
  coord_flip() + 
  ggtitle("What categories have more sub-categories?")

#deadline
deadline_by_month <- d %>% 
  filter(state %in% c("canceled", "failed", "successful")) %>%
  mutate(deadline_month = floor_date(deadline, unit = "month")) %>% 
  count(deadline_month, state, wt = NULL, sort = T)

ggplot(deadline_by_month) +
  geom_line(aes(x = deadline_month, y=n)) +
  facet_grid(state~.) +
  xlab("Deadline") + 
  ylab("Projects Count") + 
  ggtitle("Projects over time dynamics")

# goals
d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(goal <= 100000) %>%
  ggplot() +
  geom_histogram(aes(y = goal))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Goals below $100,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(goal >= 100000 & goal < 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = goal))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Goals from $100,000 till $1,000,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(goal >= 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = goal))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Goals over $1,000,000")


# pledged

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(pledged <= 100) %>%
  ggplot() +
  geom_histogram(aes(y = pledged))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged below $100")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(pledged >= 100 & pledged <= 100000) %>%
  ggplot() +
  geom_histogram(aes(y = pledged))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged from $100 till $100,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(pledged >= 100000 & pledged <= 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = pledged))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged from $100,000 till $1,000,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(pledged >= 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = pledged))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged over $1,000,000")

d %>%
  filter(state %in% c("canceled", "failed")) %>%
  filter(pledged >= 1000000)


#backers
d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers <= 50) %>%
  ggplot() +
  geom_histogram(aes(y = backers))+
  facet_grid(state~.) +
  coord_flip() + 
  xlab("Projects Count") + 
  ylab("Backers") + 
  ggtitle("Distribution of Projects with Backers below 50")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers <= 25) %>%
  group_by(state, backers)  %>%
  summarise(n = n()) %>%
  ggplot(aes(x=backers, y=n, group=state)) +
  geom_line(aes(color=state),size = 1.3)

t1 <- d %>%
  filter(state %in% c("canceled")) %>%
  filter(backers <= 25) %>%
  group_by(backers)  %>%
  summarise(n = n()) 
t2 <- d %>%
  filter(state %in% c("failed")) %>%
  filter(backers <= 25) %>%
  group_by(backers)  %>%
  summarise(n = n()) 
t3 <- d %>%
  filter(state %in% c("successful")) %>%
  filter(backers <= 25) %>%
  group_by(backers)  %>%
  summarise(n = n()) 


t1_2 <- left_join(t1,t2, by = "backers")
backers_counts <- left_join(t1_2, t3, by = "backers")

colnames(backers_counts) <- c("backers", "canceled", "failed", "successful")
backers_counts[20:25,]


d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers >= 50 & backers < 1000) %>%
  ggplot() +
  geom_histogram(aes(y = backers))+
  facet_grid(state~.) +
  coord_flip() + 
  xlab("Projects Count") + 
  ylab("Backers") + 
  ggtitle("Distribution of Projects with Backers from 50 till 1000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers >= 1000 & backers < 25000) %>%
  ggplot() +
  geom_histogram(aes(y = backers))+
  facet_grid(state~.) +
  coord_flip() + 
  xlab("Projects Count") + 
  ylab("Backers") + 
  ggtitle("Distribution of Projects with Backers from 50 till 1000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(backers >= 25000) %>%
  ggplot() +
  geom_histogram(aes(y = backers))+
  facet_grid(state~.) +
  coord_flip() + 
  xlab("Projects Count") + 
  ylab("Backers") + 
  ggtitle("Distribution of Projects with Backers from 50 till 1000")

d %>%
  group_by(state)  %>%
  summarise(max_backers = max(backers)) 
