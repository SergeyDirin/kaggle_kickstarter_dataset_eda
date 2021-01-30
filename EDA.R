
library(tidyverse)
library(lubridate)

# categories

d %>%
  count(main_category, state, wt = NULL, sort = T) %>%
  ggplot() +
  geom_bar(aes(x = reorder(main_category, n), y = n), stat = "identity") + 
  xlab("Main Category") + 
  ylab("Count")+
  coord_flip()

d %>%
  count(main_category, state, wt = NULL, sort = T) %>%
  ggplot() +
  geom_bar(aes(x = reorder(main_category, n), y = n, fill = state), stat = "identity") + 
  xlab("Main Category") + 
  ylab("Count")+
  coord_flip() +
  ggtitle("Most Frequent Categories")

#sub categories
d %>% 
  group_by(main_category) %>%
  summarise(subCats = length(unique(category))) %>%
  ggplot() +
  geom_bar(aes(x = reorder(main_category, subCats), y = subCats), stat = "identity") + 
  xlab("Main Category") + 
  ylab("SubCategory Count")+
  coord_flip() + 
  ggtitle("What categories have more sub-categories?")

#deadline
d %>% 
  filter(state %in% c("canceled", "failed", "successful")) %>%
  mutate(deadline_month = floor_date(deadline, unit = "month")) %>% 
  count(deadline_month, state, wt = NULL, sort = T) %>%
  ggplot() +
  geom_line(aes(x = deadline_month, y=n)) +
  facet_grid(state~.) +
  xlab("Deadline") + 
  ylab("Projects Count") + 
  ggtitle("Projects over time dynamics")

# goals
d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(usd_goal_real <= 100000) %>%
  ggplot() +
  geom_histogram(aes(y = usd_goal_real))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Goals below $100,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(usd_goal_real >= 100000 & usd_goal_real < 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = usd_goal_real))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Goals from $100,000 till $1,000,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(usd_goal_real >= 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = usd_goal_real))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Goals over $1,000,000")


# pledged

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(usd_pledged_real <= 1000) %>%
  ggplot() +
  geom_histogram(aes(y = usd_pledged_real))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged below $1000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(usd_pledged_real >= 1000 & usd_pledged_real <= 100000) %>%
  ggplot() +
  geom_histogram(aes(y = usd_pledged_real))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged from $1000 till $100,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(usd_pledged_real >= 100000 & usd_pledged_real <= 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = usd_pledged_real))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged from $100,000 till $1,000,000")

d %>%
  filter(state %in% c("canceled", "failed", "successful")) %>%
  filter(usd_pledged_real >= 1000000) %>%
  ggplot() +
  geom_histogram(aes(y = usd_pledged_real))+
  facet_grid(state~.) +
  coord_flip() + 
  ggtitle("Distribution of Pledged over $1,000,000")

d %>%
  filter(state %in% c("canceled", "failed")) %>%
  filter(usd_pledged_real >= 1000000)


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
  geom_line(aes(color=state),size = 1.3) +
  xlab("Projects Count") + 
  ylab("Backers") + 
  ggtitle("Distribution of Projects with Backers below 25")

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
  ggtitle("Distribution of Projects with Backers from 1000 till 25000")

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

# countries
d %>%
  count(country) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(country, n), y = n), stat = "identity") + 
  xlab("Country") + 
  ylab("Projects Count")+
  coord_flip() +
  ggtitle("Counties by number of projects")

# goals and pledged
#is being over the goal success?
d %>%
  filter(usd_pledged_real < usd_goal_real & state == "successful") %>%
  mutate(over_goal = usd_goal_real - usd_pledged_real) %>%
  select( ID, main_category, launched, usd_goal_real, over_goal, backers)
#there are 5 project that did not meet the goal but successful

# Distribution of unsuccessful projects that passed the goal
d %>%
  filter(usd_pledged_real > usd_goal_real & state != "successful") %>%
  mutate(over_goal = usd_goal_real - usd_pledged_real) %>%
  count(state) %>%
  ggplot(aes(x = state, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.25)

# what is undefined state?
d %>%
  filter(state == "undefined" & backers > 0) %>%
  mutate(over_goal = usd_goal_real - usd_pledged_real) 
# all undefined have 0 backers

# get distribution on projects without backers

d %>%
  filter(state == "undefined") %>%
  mutate(launched_year = year(launched)) %>%
  count(launched_year) %>%
  ggplot(aes(x = launched_year, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.25) +
  ggtitle("Underfined Project number by Year")



format.money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
}

# check the projects that made money
d %>%
  filter(usd_pledged_real > usd_goal_real & state == "successful") %>%
  mutate(over_goal = usd_pledged_real - usd_goal_real) %>%
  group_by(main_category) %>%
  summarise(profit = sum(over_goal)) %>%
  ggplot(aes(x = reorder(main_category, profit), y = profit)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = format_money(profit), fontface = "bold"), hjust = "inward") +
  xlab("Main Category") +
  ylab("Over Goal Profit") +
  ggtitle("Over Goal profit for successful projects")

# Underfunded for unsuccessful Projects
d %>%
  filter(usd_pledged_real < usd_goal_real & state != "successful") %>%
  mutate(over_goal = usd_goal_real - usd_pledged_real) %>%
  group_by(main_category) %>%
  summarise(profit = sum(over_goal)) %>%
  ggplot(aes(x = reorder(main_category, profit), y = profit)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = format_money(profit), fontface = "bold"), hjust = "inward") +
  xlab("Main Category") +
  ylab("Over Goal Underfunded") +
  ggtitle("Over Goal Underfunded for Unsuccessful Projects")

# comparing goal to average in the category

d %>%
  filter(state == "successful") %>%
  group_by(main_category) %>%
  summarise(mean_goal = mean(usd_goal_real)) %>%
  ggplot(aes(x = reorder(main_category, mean_goal), y = mean_goal)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = format_money(mean_goal), fontface = "bold"), hjust = "inward") +
  ylab("Average Goal") +
  xlab("Main Category") +
  geom_hline(yintercept = 9533) +
  annotate(geom = "text", y = 9200, x = 1, label = "$9,533 Overall Average", angle = 90, 
           hjust = "inward", vjust = "bottom", nudge_y = 2) +
  ggtitle("Average Goals by Categoory")

#Average Pledge for top 20 Backed Porjects
d %>% 
  arrange(desc(backers)) %>%
  top_n(20, backers) %>%
  ggplot(aes(x = backers, y = usd_pledged_real)) +
  geom_point(aes(color = main_category, shape = main_category)) + scale_color_brewer(palette="Dark2") +
  geom_text(aes( label = paste0 ("$",round(usd_pledged_real / backers, 0) )), vjust = "bottom", nudge_y = 550000) +
  xlab("Backers") +
  ylab("Pledged USD $") +
  ggtitle("Average Pledge for top 20 Backed Porjects")


#top backed Project
d %>%
  top_n(1, backers) %>%
  t()

# Photography
d %>%
  filter(main_category == "Photography") %>%
  group_by(category) %>%
  summarise(
    pledged = sum(usd_pledged_real), 
    goal = sum(usd_goal_real),
  ) %>%
  pivot_longer(!category, names_to = "pledged_goal", values_to = "amount") %>%
  ggplot(aes(x = reorder(category, amount), y = amount, fill = pledged_goal)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  geom_text(aes(label = paste(format_money(amount/1000000),"M")), size = 3.8, 
            position = position_dodge(width = 1), hjust = "inward") +
  xlab("Category") +
  ylab("Amount in USD") +
  ggtitle("Goal and Pledget Amount Comparison")

d %>%
  filter( main_category == "Photography", deadline >= mdy("1/1/2014"), deadline < mdy("1/1/2015")) %>%
  mutate(deadline_month = floor_date(deadline, unit = "month")) %>% 
  count(deadline_month, wt = NULL, sort = T) %>%
  ggplot(aes(x = deadline_month, y = n)) +
  geom_line() +
  geom_smooth() +
  xlab("Deadline") +
  ylab("Number of Projects") +
  ggtitle("Numer of Photography projects during 2014")


d %>%
  filter( main_category == "Photography") %>%
  mutate(deadline_month = floor_date(deadline, unit = "month")) %>% 
  count(deadline_month, wt = NULL, sort = T) %>%
  ggplot(aes(x = deadline_month, y = n)) +
  geom_line() +
  geom_smooth() +
  xlab("Deadline") +
  ylab("Number of Projects") +
  ggtitle("Numer of Photography projects over time")



